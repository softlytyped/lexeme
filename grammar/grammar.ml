open! Core
open Binding_syntax

type t = string list

let singulars_of reading = reading

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

let prepositions =
  String.Set.of_list
    [ "of"
    ; "at"
    ; "from"
    ; "to"
    ; "in"
    ; "on"
    ; "by"
    ; "for"
    ; "with"
    ; "without"
    ; "as"
    ; "into"
    ; "onto"
    ; "per"
    ; "over"
    ; "under"
    ; "between"
    ; "through"
    ; "before"
    ; "after"
    ; "among"
    ; "within"
    ; "across"
    ; "along"
    ; "beside"
    ; "above"
    ; "below"
    ; "against"
    ]
;;

(* An adjective may have adverbs of its own. A noun may qualify the next noun, as register
   qualifies bit in register_bit. *)
let rec are_modifiers lexicon ~adjective = function
  | [] -> true
  | word :: rest
    when Lexicon.is_adjective lexicon word || (adjective && Lexicon.is_adverb lexicon word)
    -> are_modifiers lexicon ~adjective:true rest
  | word :: rest when Lexicon.is_noun lexicon word ->
    are_modifiers lexicon ~adjective:false rest
  | _ -> false
;;

let noun_of lexicon words =
  match List.rev words with
  | head :: modifiers
    when Lexicon.is_noun lexicon head && are_modifiers lexicon ~adjective:false modifiers
    -> Some (List.rev modifiers, head)
  | _ -> None
;;

let phrase_of words = List.split_while words ~f:(Fn.non (Set.mem prepositions))

let rec is_noun_phrase lexicon words =
  let subject, rest = phrase_of words in
  Option.is_some (noun_of lexicon subject)
  &&
  match rest with
  | [] -> true
  | _preposition :: complement -> is_noun_phrase lexicon complement
;;

let rec is_verb_phrase lexicon = function
  | [] -> false
  | word :: rest ->
    (Lexicon.is_verb lexicon word && List.for_all rest ~f:(Lexicon.is_adverb lexicon))
    || (Lexicon.is_adverb lexicon word && is_verb_phrase lexicon rest)
;;

let rec are_verbs lexicon words =
  let phrase, rest =
    List.split_while words ~f:(fun word ->
      not (List.mem [ "and"; "or"; "but" ] word ~equal:String.equal))
  in
  is_verb_phrase lexicon phrase
  &&
  match rest with
  | [] -> true
  | _conjunction :: tail -> are_verbs lexicon tail
;;

let is_adjective_phrase lexicon words =
  match List.rev words with
  | head :: modifiers ->
    Lexicon.is_adjective lexicon head
    && List.for_all modifiers ~f:(Lexicon.is_adverb lexicon)
  | [] -> false
;;

type auxiliary =
  | Copula
  | Possession
  | Modal

let auxiliary_of = function
  | "is" | "are" -> Some Copula
  | "has" | "have" -> Some Possession
  | "does" | "do" | "can" | "may" | "must" | "should" | "will" -> Some Modal
  | _ -> None
;;

let is_predicate name =
  match String.lsplit2 (stem_of name) ~on:'_' with
  | Some (word, _) -> Option.is_some (auxiliary_of (String.lowercase word))
  | None -> false
;;

let is_complement lexicon auxiliary words =
  match auxiliary, words with
  | Copula, preposition :: rest when Set.mem prepositions preposition ->
    is_noun_phrase lexicon rest
  | Copula, _ -> is_noun_phrase lexicon words || is_adjective_phrase lexicon words
  | Possession, _ -> is_noun_phrase lexicon words
  | Modal, _ -> are_verbs lexicon words
;;

let singulars_at lexicon name ~modifiers ~head =
  let prefix =
    String.length name - String.length (String.lstrip name ~drop:(Char.equal '_'))
  in
  let start =
    List.fold modifiers ~init:prefix ~f:(fun offset word ->
      offset + String.length word + 1)
  in
  let spelling = String.sub name ~pos:start ~len:(String.length head) in
  let suffix = String.drop_prefix name (start + String.length head) in
  Lexicon.singulars_of lexicon head
  |> List.map ~f:(fun singular ->
    let singular =
      match String.equal (String.uppercase spelling) spelling with
      | true -> String.uppercase singular
      | false when Char.is_uppercase spelling.[0] -> String.capitalize singular
      | false -> singular
    in
    String.prefix name start ^ singular ^ suffix)
;;

let read lexicon role name =
  let stem = stem_of name in
  let words = String.split (String.lowercase stem) ~on:'_' in
  match role, words with
  | _, _ when List.exists words ~f:String.is_empty -> None
  | _, word :: (_ :: _ as complement) when Option.is_some (auxiliary_of word) ->
    let%bind.Option auxiliary = auxiliary_of word in
    Option.some_if
      (equal_role role Function && is_complement lexicon auxiliary complement)
      []
  | Variable, _ -> Option.some_if (is_noun_phrase lexicon words) []
  | Function, preposition :: complement when Set.mem prepositions preposition ->
    Option.some_if (is_noun_phrase lexicon complement) []
  | Function, _ ->
    let subject, rest = phrase_of words in
    (match rest with
     | [] -> Option.some_if (are_verbs lexicon subject) []
     | _preposition :: complement ->
       let%bind.Option modifiers, head = noun_of lexicon subject in
       let%map.Option () =
         Option.some_if (List.is_empty complement || is_noun_phrase lexicon complement) ()
       in
       singulars_at lexicon name ~modifiers ~head)
;;
