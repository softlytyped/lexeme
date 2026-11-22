open! Core

(* A regular expression over whole words. A word can satisfy several parts of speech. At a
   repetition we visit each suffix once, even if it has several readings. *)
module Phrase = struct
  type t =
    | Word of (string -> bool)
    | Sequence of t list
    | Choice of t list
    | Many of t

  let rec accept phrase words position finish =
    match phrase with
    | Word reading ->
      position < Array.length words && reading words.(position) && finish (position + 1)
    | Choice phrases ->
      List.exists phrases ~f:(fun phrase -> accept phrase words position finish)
    | Sequence phrases ->
      let dispatch =
        List.fold_right phrases ~init:finish ~f:(fun phrase rest position ->
          accept phrase words position rest)
      in
      dispatch position
    | Many phrase ->
      let positions = Hash_set.create (module Int) in
      let rec repeat position =
        match Hash_set.mem positions position with
        | true -> false
        | false ->
          Hash_set.add positions position;
          finish position || accept phrase words position repeat
      in
      repeat position
  ;;

  let does_match phrase words =
    let words = Array.of_list_map words ~f:String.lowercase in
    accept phrase words 0 (Int.equal (Array.length words))
  ;;

  let chain phrase separator = Sequence [ phrase; Many (Sequence [ separator; phrase ]) ]
end

let prepositions =
  String.split
    "of at from to in on by for with without as into onto per over under between through \
     before after among within across along beside above below against"
    ~on:' '
  |> String.Set.of_list
;;

let is_preposition word = Set.mem prepositions (String.lowercase word)

let is_conjunction = function
  | "and" | "or" | "but" -> true
  | _ -> false
;;

type t =
  { lexicon : Lexicon.t
  ; subject : Phrase.t
  ; nominal : Phrase.t
  ; action : Phrase.t
  ; adjective : Phrase.t
  }

let grammar_of lexicon =
  let open Phrase in
  let word reading ~separator =
    Word (fun word -> (not (separator word)) && reading lexicon word)
  in
  let noun = word Lexicon.is_noun ~separator:is_preposition in
  let modifiers_of separator = Many (word Lexicon.is_adverb ~separator) in
  let adjective =
    Sequence
      [ modifiers_of is_preposition; word Lexicon.is_adjective ~separator:is_preposition ]
  in
  let subject = Sequence [ Many (Choice [ noun; adjective ]); noun ] in
  let modifiers = modifiers_of is_conjunction in
  let verb =
    Sequence [ modifiers; word Lexicon.is_verb ~separator:is_conjunction; modifiers ]
  in
  { lexicon
  ; subject
  ; nominal = chain subject (Word is_preposition)
  ; action = chain verb (Word is_conjunction)
  ; adjective
  }
;;

let stem_of name =
  String.lstrip name ~drop:(Char.equal '_') |> String.rstrip ~drop:(Char.equal '\'')
;;

let is_letter name =
  let stem = stem_of name in
  match String.is_empty stem with
  | true -> false
  | false ->
    let letter = Stdlib.String.get_utf_8_uchar stem 0 in
    Stdlib.Uchar.utf_decode_is_valid letter
    && Stdlib.Uchar.utf_decode_length letter = String.length stem
    && Uucp.Alpha.is_alphabetic (Stdlib.Uchar.utf_decode_uchar letter)
;;

type auxiliary =
  | Copula
  | Possession
  | Modal

type name =
  { spelling : string
  ; words : string list
  ; predicate : (auxiliary * string list) option
  }

let read spelling =
  let words = String.split (stem_of spelling) ~on:'_' in
  let predicate =
    match words with
    | first :: (_ :: _ as rest) ->
      (match String.lowercase first with
       | "is" | "are" -> Some (Copula, rest)
       | "has" | "have" -> Some (Possession, rest)
       | "does" | "do" | "can" | "may" | "must" | "should" | "will" -> Some (Modal, rest)
       | _ -> None)
    | _ -> None
  in
  { spelling; words; predicate }
;;

let is_predicate name = Option.is_some name.predicate

let is_noun grammar name =
  (not (is_predicate name)) && Phrase.does_match grammar.nominal name.words
;;

let is_complement grammar auxiliary words =
  match auxiliary, words with
  | Modal, _ -> Phrase.does_match grammar.action words
  | Possession, _ -> Phrase.does_match grammar.nominal words
  | Copula, first :: tail when is_preposition first ->
    Phrase.does_match grammar.nominal tail
  | Copula, _ -> Phrase.does_match (Choice [ grammar.nominal; grammar.adjective ]) words
;;

let inflect word singular =
  match String.equal word (String.uppercase word) with
  | true -> String.uppercase singular
  | false when Char.is_uppercase word.[0] -> String.capitalize singular
  | false -> singular
;;

let singulars_of lexicon name ~subject ~complement =
  match List.rev subject with
  | [] -> []
  | head :: modifiers ->
    let noun = String.lowercase head in
    List.filter (Lexicon.singulars_of lexicon noun) ~f:(Fn.non (String.equal noun))
    |> List.map ~f:(fun singular ->
      let words = List.rev_append modifiers (inflect head singular :: complement) in
      String.substr_replace_first
        name.spelling
        ~pattern:(stem_of name.spelling)
        ~with_:(String.concat ~sep:"_" words))
;;

let function_of grammar name =
  let accept judgement = Option.some_if judgement [] in
  match name.predicate, name.words with
  | Some (auxiliary, complement), _ -> accept (is_complement grammar auxiliary complement)
  | None, first :: tail when is_preposition first ->
    accept (Phrase.does_match grammar.nominal tail)
  | None, words ->
    let subject, complement = List.split_while words ~f:(Fn.non is_preposition) in
    (match complement with
     | [] -> accept (Phrase.does_match grammar.action subject)
     | _ :: tail ->
       let%map.Option () =
         Option.some_if
           (Phrase.does_match grammar.subject subject
            && (List.is_empty tail || Phrase.does_match grammar.nominal tail))
           ()
       in
       singulars_of grammar.lexicon name ~subject ~complement)
;;
