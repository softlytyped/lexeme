open! Core

module Vocabulary = struct
  type t =
    { nouns : string list [@sexp.list]
    ; verbs : string list [@sexp.list]
    ; adjectives : string list [@sexp.list]
    ; adverbs : string list [@sexp.list]
    ; plurals : (string * string) list [@sexp.list]
    }
  [@@deriving of_sexp]

  let empty = { nouns = []; verbs = []; adjectives = []; adverbs = []; plurals = [] }

  let words_of { nouns; verbs; adjectives; adverbs; plurals } =
    let inflections =
      List.concat_map plurals ~f:(fun (plural, singular) -> [ plural; singular ])
    in
    List.concat [ nouns; verbs; adjectives; adverbs; inflections ]
  ;;
end

type t =
  { nouns : String.Set.t
  ; verbs : String.Set.t
  ; adjectives : String.Set.t
  ; adverbs : String.Set.t
  ; singulars : String.Set.t String.Map.t
  }

let empty =
  { nouns = String.Set.empty
  ; verbs = String.Set.empty
  ; adjectives = String.Set.empty
  ; adverbs = String.Set.empty
  ; singulars = String.Map.empty
  }
;;

let inflect lexicon (plural, singular) =
  let add_singular readings =
    Set.add (Option.value readings ~default:String.Set.empty) singular
  in
  { lexicon with
    nouns = Set.add lexicon.nouns singular
  ; singulars = Map.update lexicon.singulars plural ~f:add_singular
  }
;;

let add lexicon (vocabulary : Vocabulary.t) =
  let union words entries = Set.union words (String.Set.of_list entries) in
  let lexicon =
    { lexicon with
      nouns = union lexicon.nouns vocabulary.nouns
    ; verbs = union lexicon.verbs vocabulary.verbs
    ; adjectives = union lexicon.adjectives vocabulary.adjectives
    ; adverbs = union lexicon.adverbs vocabulary.adverbs
    }
  in
  List.fold vocabulary.plurals ~init:lexicon ~f:inflect
;;

(* Each line of the table is a part of speech followed by its word, or "plural" followed
   by a plural and one of its singulars. *)
let vocabulary_of_table table : Vocabulary.t =
  let lines = String.split_lines table in
  let words_of tag = List.filter_map lines ~f:(String.chop_prefix ~prefix:(tag ^ " ")) in
  { nouns = words_of "noun"
  ; verbs = words_of "verb"
  ; adjectives = words_of "adj"
  ; adverbs = words_of "adv"
  ; plurals = List.filter_map (words_of "plural") ~f:(String.lsplit2 ~on:' ')
  }
;;

let english = lazy (add empty (vocabulary_of_table English.table))

let is_word word =
  match String.to_list word with
  | [] -> false
  | first :: _ ->
    Char.is_lowercase first
    && String.for_all word ~f:(fun letter ->
      Char.is_lowercase letter || Char.is_digit letter)
;;

let extend lexicon vocabulary =
  match List.find (Vocabulary.words_of vocabulary) ~f:(Fn.non is_word) with
  | Some word -> Or_error.error_s [%message "expected one lowercase vocabulary word" word]
  | None -> Ok (add lexicon vocabulary)
;;

let is_verb lexicon = Set.mem lexicon.verbs
let is_adverb lexicon = Set.mem lexicon.adverbs

(* A verb also supplies adjectival participles. We undo each spelling change, as in
   enabled, carried, dying and stopped, and look for an attested root verb. *)
let roots_of_participle word =
  let respell stem ~ending ~root_ending =
    Option.map (String.chop_suffix stem ~suffix:ending) ~f:(fun root ->
      root ^ root_ending)
  in
  let undouble stem =
    match List.rev (String.to_list stem) with
    | last :: before :: _ when Char.equal last before -> Some (String.drop_suffix stem 1)
    | _ -> None
  in
  match String.chop_suffix word ~suffix:"ed", String.chop_suffix word ~suffix:"ing" with
  | Some stem, _ ->
    [ Some stem
    ; Some (stem ^ "e")
    ; respell stem ~ending:"i" ~root_ending:"y"
    ; undouble stem
    ]
  | None, Some stem ->
    [ Some stem
    ; Some (stem ^ "e")
    ; respell stem ~ending:"y" ~root_ending:"ie"
    ; undouble stem
    ]
  | None, None -> []
;;

let is_adjective lexicon word =
  Set.mem lexicon.adjectives word
  || List.exists (roots_of_participle word) ~f:(Option.exists ~f:(is_verb lexicon))
;;

let is_vowel letter = String.mem "aeiou" letter

let regular_plural_of noun =
  match List.rev (String.to_list noun) with
  | 'y' :: consonant :: _ when not (is_vowel consonant) ->
    String.drop_suffix noun 1 ^ "ies"
  | ('s' | 'x' | 'z') :: _ | 'h' :: ('c' | 's') :: _ -> noun ^ "es"
  | _ -> noun ^ "s"
;;

let regular_singulars_of lexicon plural =
  [ "ies", "y"; "es", ""; "s", "" ]
  |> List.filter_map ~f:(fun (suffix, ending) ->
    let%bind.Option stem = String.chop_suffix plural ~suffix in
    let noun = stem ^ ending in
    Option.some_if
      (Set.mem lexicon.nouns noun && String.equal (regular_plural_of noun) plural)
      noun)
  |> List.dedup_and_sort ~compare:String.compare
;;

let singulars_of lexicon plural =
  match Map.find lexicon.singulars plural with
  | Some readings -> Set.to_list readings
  | None -> regular_singulars_of lexicon plural
;;

let is_noun lexicon word =
  Set.mem lexicon.nouns word || not (List.is_empty (singulars_of lexicon word))
;;
