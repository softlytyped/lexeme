open! Core

(* A regular expression over whole words. A word can satisfy several parts of speech. At a
   repetition we visit each position once, even where its words have several readings. *)
module Phrase = struct
  type t =
    | Word of (string -> bool)
    | Sequence of t list
    | Choice of t list
    | Repetition of t

  (* [is_complete position] answers whether the words from [position] on complete the
     match. *)
  let rec does_accept phrase words position ~is_complete =
    match phrase with
    | Word does_fit -> does_accept_once does_fit words position ~is_complete
    | Choice phrases ->
      List.exists phrases ~f:(fun phrase ->
        does_accept phrase words position ~is_complete)
    | Sequence phrases -> does_accept_successively phrases words position ~is_complete
    | Repetition phrase -> does_accept_repeatedly phrase words position ~is_complete

  and does_accept_once does_fit words position ~is_complete =
    position < Array.length words
    && does_fit words.(position)
    && is_complete (position + 1)

  and does_accept_successively phrases words position ~is_complete =
    match phrases with
    | [] -> is_complete position
    | phrase :: rest ->
      does_accept phrase words position ~is_complete:(fun position ->
        does_accept_successively rest words position ~is_complete)

  and does_accept_repeatedly phrase words position ~is_complete =
    let visited_positions = Hash_set.create (module Int) in
    let rec does_continue position =
      match Hash_set.strict_add visited_positions position with
      | Duplicate -> false
      | Ok ->
        is_complete position
        || does_accept phrase words position ~is_complete:does_continue
    in
    does_continue position
  ;;

  let does_match phrase words =
    let words = Array.of_list_map words ~f:String.lowercase in
    does_accept phrase words 0 ~is_complete:(Int.equal (Array.length words))
  ;;

  let chain phrase separator =
    Sequence [ phrase; Repetition (Sequence [ separator; phrase ]) ]
  ;;
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

let word_of lexicon is_kind ~is_separator =
  Phrase.Word (fun word -> (not (is_separator word)) && is_kind lexicon word)
;;

let adjective_phrase_of lexicon =
  let adverbs =
    Phrase.Repetition (word_of lexicon Lexicon.is_adverb ~is_separator:is_preposition)
  in
  Phrase.Sequence
    [ adverbs; word_of lexicon Lexicon.is_adjective ~is_separator:is_preposition ]
;;

let noun_phrase_of lexicon =
  let noun = word_of lexicon Lexicon.is_noun ~is_separator:is_preposition in
  Phrase.Sequence [ Repetition (Choice [ noun; adjective_phrase_of lexicon ]); noun ]
;;

let verb_phrase_of lexicon =
  let adverbs =
    Phrase.Repetition (word_of lexicon Lexicon.is_adverb ~is_separator:is_conjunction)
  in
  let verb = word_of lexicon Lexicon.is_verb ~is_separator:is_conjunction in
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
    String.rstrip (String.lstrip spelling ~drop:(Char.equal '_')) ~drop:(Char.equal '\'')
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

  module Form = struct
    type t =
      | Question
      | Statement
  end

  let form_of name =
    match name.predicate with
    | Some _ -> Form.Question
    | None -> Statement
  ;;

  let is_single_letter stem =
    let decoding = Stdlib.String.get_utf_8_uchar stem 0 in
    Stdlib.Uchar.utf_decode_is_valid decoding
    && Stdlib.Uchar.utf_decode_length decoding = String.length stem
    && Uucp.Alpha.is_alphabetic (Stdlib.Uchar.utf_decode_uchar decoding)
  ;;

  let is_letter name =
    let stem = stem_of name.spelling in
    (not (String.is_empty stem)) && is_single_letter stem
  ;;

  (* We keep the spelling around the stem, such as a leading underscore. *)
  let spelling_with name words =
    let stem = String.concat ~sep:"_" words in
    String.substr_replace_first name.spelling ~pattern:(stem_of name.spelling) ~with_:stem
  ;;
end

module Reading = struct
  type t =
    | Noun_phrase
    | Verb_phrase
    | Predicate
    | Nominal
  [@@deriving equal, sexp_of]
end

let is_object grammar words =
  List.is_empty words || Phrase.does_match grammar.nominal words
;;

(* A verb phrase may end in a prepositional phrase, as in [drain_into], whether it names a
   command or follows a modal verb, as in [does_drain_into]. *)
let is_verbal grammar words =
  match List.split_while words ~f:(Fn.non is_preposition) with
  | verbs, [] -> Phrase.does_match grammar.verb_phrase verbs
  | verbs, _ :: rest ->
    Phrase.does_match grammar.verb_phrase verbs && is_object grammar rest
;;

let is_complement grammar ({ auxiliary; complement } : Name.predicate) =
  match auxiliary, complement with
  | Modal, _ -> is_verbal grammar complement
  | Possession, _ -> Phrase.does_match grammar.nominal complement
  | Copula, first :: rest when is_preposition first ->
    Phrase.does_match grammar.nominal rest
  | Copula, _ ->
    Phrase.does_match (Choice [ grammar.nominal; grammar.adjective_phrase ]) complement
;;

(* We spell a singular in the case of the plural it replaces. *)
let inflect plural singular =
  match String.to_list plural with
  | _ when String.equal plural (String.uppercase plural) -> String.uppercase singular
  | first :: _ when Char.is_uppercase first -> String.capitalize singular
  | _ -> singular
;;

let other_singulars_of grammar head =
  let noun = String.lowercase head in
  List.filter (Lexicon.singulars_of grammar.lexicon noun) ~f:(Fn.non (String.equal noun))
;;

let singulars_of grammar (name : Name.t) =
  let subject, complement = List.split_while name.words ~f:(Fn.non is_preposition) in
  match List.rev subject, complement with
  | [], _ | _, [] -> []
  | head :: modifiers, _ :: _ ->
    List.map (other_singulars_of grammar head) ~f:(fun singular ->
      let words = inflect head singular :: complement in
      Name.spelling_with name (List.rev_append modifiers words))
;;

let is_noun_phrase grammar (name : Name.t) = Phrase.does_match grammar.nominal name.words
let is_verb_phrase grammar (name : Name.t) = is_verbal grammar name.words

let is_nominal grammar (name : Name.t) =
  match List.split_while name.words ~f:(Fn.non is_preposition) with
  | _, [] -> false
  | [], _ :: rest -> Phrase.does_match grammar.nominal rest
  | subject, _ :: rest ->
    Phrase.does_match grammar.noun_phrase subject && is_object grammar rest
;;

let statement_readings_of grammar name =
  List.filter_map
    [ is_noun_phrase, Reading.Noun_phrase
    ; is_verb_phrase, Verb_phrase
    ; is_nominal, Nominal
    ]
    ~f:(fun (is_reading, reading) -> Option.some_if (is_reading grammar name) reading)
;;

let readings_of grammar (name : Name.t) =
  match name.predicate with
  | Some predicate ->
    Option.to_list (Option.some_if (is_complement grammar predicate) Reading.Predicate)
  | None -> statement_readings_of grammar name
;;
