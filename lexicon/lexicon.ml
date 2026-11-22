open! Core

type vocabulary =
  { nouns : string list [@sexp.list]
  ; verbs : string list [@sexp.list]
  ; adjectives : string list [@sexp.list]
  ; adverbs : string list [@sexp.list]
  ; plurals : (string * string) list [@sexp.list]
  }
[@@deriving of_sexp]

type t =
  { nouns : String.Set.t
  ; verbs : String.Set.t
  ; adjectives : String.Set.t
  ; adverbs : String.Set.t
  ; singulars : String.Set.t String.Map.t
  }

let inflect lexicon ~plural ~singular =
  let singulars =
    Map.update lexicon.singulars plural ~f:(fun readings ->
      Set.add (Option.value readings ~default:String.Set.empty) singular)
  in
  { lexicon with nouns = Set.add lexicon.nouns singular; singulars }
;;

let english =
  lazy
    (let empty =
       { nouns = String.Set.empty
       ; verbs = String.Set.empty
       ; adjectives = String.Set.empty
       ; adverbs = String.Set.empty
       ; singulars = String.Map.empty
       }
     in
     String.split_lines English.vocabulary
     |> List.fold ~init:empty ~f:(fun lexicon line ->
       match String.split line ~on:' ' with
       | [ "noun"; word ] -> { lexicon with nouns = Set.add lexicon.nouns word }
       | [ "verb"; word ] -> { lexicon with verbs = Set.add lexicon.verbs word }
       | [ "adj"; word ] -> { lexicon with adjectives = Set.add lexicon.adjectives word }
       | [ "adv"; word ] -> { lexicon with adverbs = Set.add lexicon.adverbs word }
       | [ "plural"; plural; singular ] -> inflect lexicon ~plural ~singular
       | _ -> lexicon))
;;

let is_word word =
  (not (String.is_empty word))
  && String.for_all word ~f:(fun letter ->
    Char.is_lowercase letter || Char.is_digit letter)
  && Char.is_lowercase word.[0]
;;

let extend lexicon (vocabulary : vocabulary) =
  let { nouns; verbs; adjectives; adverbs; plurals } = vocabulary in
  let words =
    List.concat
      [ nouns
      ; verbs
      ; adjectives
      ; adverbs
      ; List.concat_map plurals ~f:(fun (plural, singular) -> [ plural; singular ])
      ]
  in
  match List.find words ~f:(Fn.non is_word) with
  | Some word -> Or_error.error_s [%message "expected one lowercase vocabulary word" word]
  | None ->
    let add words entries = List.fold entries ~init:words ~f:Set.add in
    let dictionary =
      { lexicon with
        nouns = add lexicon.nouns nouns
      ; verbs = add lexicon.verbs verbs
      ; adjectives = add lexicon.adjectives adjectives
      ; adverbs = add lexicon.adverbs adverbs
      }
    in
    Ok
      (List.fold plurals ~init:dictionary ~f:(fun lexicon (plural, singular) ->
         inflect lexicon ~plural ~singular))
;;

let is_verb lexicon = Set.mem lexicon.verbs
let is_adverb lexicon = Set.mem lexicon.adverbs

(* A verb also supplies adjectival participles, such as enabled and running. We check each
   spelling change against an attested root verb. *)
let is_participle lexicon word =
  let has_root stem =
    let length = String.length stem in
    is_verb lexicon stem
    || is_verb lexicon (stem ^ "e")
    || (length > 1
        && Char.equal stem.[length - 1] stem.[length - 2]
        && is_verb lexicon (String.drop_suffix stem 1))
  in
  match String.chop_suffix word ~suffix:"ed", String.chop_suffix word ~suffix:"ing" with
  | Some stem, _ ->
    has_root stem
    || Option.exists (String.chop_suffix stem ~suffix:"i") ~f:(fun root ->
      is_verb lexicon (root ^ "y"))
  | _, Some stem ->
    has_root stem
    || Option.exists (String.chop_suffix stem ~suffix:"y") ~f:(fun root ->
      is_verb lexicon (root ^ "ie"))
  | _ -> false
;;

let is_adjective lexicon word =
  Set.mem lexicon.adjectives word || is_participle lexicon word
;;

let regular_plural_of noun =
  match String.chop_suffix noun ~suffix:"y" with
  | Some stem
    when String.exists (String.suffix stem 1) ~f:(fun letter ->
           not (String.mem "aeiou" letter)) -> stem ^ "ies"
  | _
    when List.exists [ "s"; "x"; "z"; "ch"; "sh" ] ~f:(fun suffix ->
           String.is_suffix noun ~suffix) -> noun ^ "es"
  | _ -> noun ^ "s"
;;

let singulars_of lexicon plural =
  match Map.find lexicon.singulars plural with
  | Some readings -> Set.to_list readings
  | None ->
    [ "ies", "y"; "es", ""; "s", "" ]
    |> List.filter_map ~f:(fun (suffix, ending) ->
      let%bind.Option stem = String.chop_suffix plural ~suffix in
      let noun = stem ^ ending in
      Option.some_if
        (Set.mem lexicon.nouns noun && String.equal (regular_plural_of noun) plural)
        noun)
    |> List.dedup_and_sort ~compare:String.compare
;;

let is_noun lexicon word =
  Set.mem lexicon.nouns word || not (List.is_empty (singulars_of lexicon word))
;;
