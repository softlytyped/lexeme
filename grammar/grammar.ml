open! Core

(* A regular expression over whole words. A word can satisfy several parts of speech. At a
   repetition we visit each position once, even where its words have several readings. *)
module Phrase = struct
  type t =
    | Word of (string -> bool)
    | Sequence of t list
    | Choice of t list
    | Many of t

  let rec does_accept phrase words position ~continuation =
    match phrase with
    | Word reading ->
      position < Array.length words
      && reading words.(position)
      && continuation (position + 1)
    | Choice phrases ->
      List.exists phrases ~f:(fun phrase ->
        does_accept phrase words position ~continuation)
    | Sequence phrases -> does_accept_in_turn phrases words position ~continuation
    | Many phrase -> does_accept_repeatedly phrase words position ~continuation

  and does_accept_in_turn phrases words position ~continuation =
    match phrases with
    | [] -> continuation position
    | phrase :: rest ->
      does_accept phrase words position ~continuation:(fun position ->
        does_accept_in_turn rest words position ~continuation)

  and does_accept_repeatedly phrase words position ~continuation =
    let visited = Hash_set.create (module Int) in
    let rec does_continue position =
      match Hash_set.strict_add visited position with
      | Duplicate -> false
      | Ok ->
        continuation position
        || does_accept phrase words position ~continuation:does_continue
    in
    does_continue position
  ;;

  let does_match phrase words =
    let words = Array.of_list_map words ~f:String.lowercase in
    does_accept phrase words 0 ~continuation:(Int.equal (Array.length words))
  ;;

  let chain phrase separator = Sequence [ phrase; Many (Sequence [ separator; phrase ]) ]
end

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

let is_preposition word = Set.mem prepositions (String.lowercase word)

let is_conjunction word =
  match String.lowercase word with
  | "and" | "or" | "but" -> true
  | _ -> false
;;

module Auxiliary = struct
  type t =
    | Copula
    | Possession
    | Modal

  let of_word word =
    match String.lowercase word with
    | "is" | "are" -> Some Copula
    | "has" | "have" -> Some Possession
    | "does" | "do" | "can" | "may" | "must" | "should" | "will" -> Some Modal
    | _ -> None
  ;;
end

type t =
  { lexicon : Lexicon.t
  ; noun_phrase : Phrase.t
  ; nominal : Phrase.t
  ; verb_phrase : Phrase.t
  ; adjective_phrase : Phrase.t
  }

let word_of lexicon reading ~separator =
  Phrase.Word (fun word -> (not (separator word)) && reading lexicon word)
;;

let adjective_phrase_of lexicon =
  let adverbs =
    Phrase.Many (word_of lexicon Lexicon.is_adverb ~separator:is_preposition)
  in
  Phrase.Sequence
    [ adverbs; word_of lexicon Lexicon.is_adjective ~separator:is_preposition ]
;;

let noun_phrase_of lexicon =
  let noun = word_of lexicon Lexicon.is_noun ~separator:is_preposition in
  Phrase.Sequence [ Many (Choice [ noun; adjective_phrase_of lexicon ]); noun ]
;;

let verb_phrase_of lexicon =
  let adverbs =
    Phrase.Many (word_of lexicon Lexicon.is_adverb ~separator:is_conjunction)
  in
  let verb = word_of lexicon Lexicon.is_verb ~separator:is_conjunction in
  Phrase.chain (Sequence [ adverbs; verb; adverbs ]) (Word is_conjunction)
;;

(* A nominal joins noun phrases with prepositions, as in [start_of_line]. *)
let of_lexicon lexicon =
  let noun_phrase = noun_phrase_of lexicon in
  { lexicon
  ; noun_phrase
  ; nominal = Phrase.chain noun_phrase (Word is_preposition)
  ; verb_phrase = verb_phrase_of lexicon
  ; adjective_phrase = adjective_phrase_of lexicon
  }
;;

module Name = struct
  type predicate =
    { auxiliary : Auxiliary.t
    ; complement : string list
    }

  type t =
    { spelling : string
    ; words : string list
    ; predicate : predicate option
    }

  let stem_of spelling =
    String.lstrip spelling ~drop:(Char.equal '_') |> String.rstrip ~drop:(Char.equal '\'')
  ;;

  let predicate_of words =
    match words with
    | first :: (_ :: _ as complement) ->
      Option.map (Auxiliary.of_word first) ~f:(fun auxiliary -> { auxiliary; complement })
    | _ -> None
  ;;

  let of_spelling spelling =
    let words = String.split (stem_of spelling) ~on:'_' in
    { spelling; words; predicate = predicate_of words }
  ;;

  let has_auxiliary name = Option.is_some name.predicate

  let is_letter name =
    let stem = stem_of name.spelling in
    match String.is_empty stem with
    | true -> false
    | false ->
      let decoding = Stdlib.String.get_utf_8_uchar stem 0 in
      Stdlib.Uchar.utf_decode_is_valid decoding
      && Stdlib.Uchar.utf_decode_length decoding = String.length stem
      && Uucp.Alpha.is_alphabetic (Stdlib.Uchar.utf_decode_uchar decoding)
  ;;

  (* We keep the spelling around the stem, such as a leading underscore. *)
  let respell name words =
    String.substr_replace_first
      name.spelling
      ~pattern:(stem_of name.spelling)
      ~with_:(String.concat ~sep:"_" words)
  ;;
end

module Reading = struct
  type t =
    | Noun_phrase
    | Verb_phrase
    | Predicate
    | Nominal of string list
  [@@deriving equal, sexp_of]
end

let is_complement grammar ({ auxiliary; complement } : Name.predicate) =
  match auxiliary, complement with
  | Modal, _ -> Phrase.does_match grammar.verb_phrase complement
  | Possession, _ -> Phrase.does_match grammar.nominal complement
  | Copula, first :: rest when is_preposition first ->
    Phrase.does_match grammar.nominal rest
  | Copula, _ ->
    Phrase.does_match (Choice [ grammar.nominal; grammar.adjective_phrase ]) complement
;;

(* We spell a singular in the case of the plural it replaces. *)
let inflect plural singular =
  match String.equal plural (String.uppercase plural) with
  | true -> String.uppercase singular
  | false when Char.is_uppercase plural.[0] -> String.capitalize singular
  | false -> singular
;;

let singulars_of grammar name ~subject ~complement =
  match List.rev subject with
  | [] -> []
  | head :: modifiers ->
    let noun = String.lowercase head in
    List.filter
      (Lexicon.singulars_of grammar.lexicon noun)
      ~f:(Fn.non (String.equal noun))
    |> List.map ~f:(fun singular ->
      Name.respell name (List.rev_append modifiers (inflect head singular :: complement)))
;;

let noun_reading grammar (name : Name.t) =
  Option.some_if (Phrase.does_match grammar.nominal name.words) Reading.Noun_phrase
;;

let verb_reading grammar (name : Name.t) =
  Option.some_if
    ((not (List.exists name.words ~f:is_preposition))
     && Phrase.does_match grammar.verb_phrase name.words)
    Reading.Verb_phrase
;;

let nominal_reading grammar (name : Name.t) =
  let is_object words = List.is_empty words || Phrase.does_match grammar.nominal words in
  match List.split_while name.words ~f:(Fn.non is_preposition) with
  | _, [] -> None
  | [], _ :: rest ->
    Option.some_if (Phrase.does_match grammar.nominal rest) (Reading.Nominal [])
  | subject, (_ :: rest as complement) ->
    let%map.Option () =
      Option.some_if (Phrase.does_match grammar.noun_phrase subject && is_object rest) ()
    in
    Reading.Nominal (singulars_of grammar name ~subject ~complement)
;;

let readings_of grammar (name : Name.t) =
  match name.predicate with
  | Some predicate ->
    Option.to_list (Option.some_if (is_complement grammar predicate) Reading.Predicate)
  | None ->
    List.filter_opt
      [ noun_reading grammar name
      ; verb_reading grammar name
      ; nominal_reading grammar name
      ]
;;
