open! Core

module Extent = struct
  module T = struct
    type t =
      { start : int
      ; stop : int
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let create ~start ~stop = { start; stop }
  let start_of extent = extent.start
  let stop_of extent = extent.stop
end

module Comment = struct
  type t =
    { extent : Extent.t
    ; body : string
    }

  let create ~extent ~body = { extent; body }
  let extent_of comment = comment.extent
  let body_of comment = comment.body
end

module Site = struct
  type t =
    { path : string
    ; line : int
    ; column : int
    }
  [@@deriving compare]

  let path_of site = site.path
  let line_of site = site.line
  let column_of site = site.column
  let to_string site = sprintf "%s:%d:%d" site.path site.line site.column
end

(* We end [line_starts] with the length of the text, so that the line at [index] spans the
   offsets from [line_starts.(index)] up to [line_starts.(index + 1)]. *)
type t =
  { path : string
  ; text : string
  ; line_starts : int array
  ; code_lines : bool array
  ; comments : string Int.Map.t
  }

let line_starts_of text =
  let breaks = String.substr_index_all text ~pattern:"\n" ~may_overlap:false in
  Array.of_list ((0 :: List.map breaks ~f:Int.succ) @ [ String.length text ])
;;

let comment_mask_of text comments =
  let mask = Array.create ~len:(String.length text) false in
  List.iter comments ~f:(fun ({ extent = { start; stop }; body = _ } : Comment.t) ->
    Array.fill mask ~pos:start ~len:(stop - start) true);
  mask
;;

let code_lines_of text line_starts comments =
  let mask = comment_mask_of text comments in
  let is_code offset = not (mask.(offset) || Char.is_whitespace text.[offset]) in
  let offsets_of line = Sequence.range line_starts.(line) line_starts.(line + 1) in
  Array.init
    (Array.length line_starts - 1)
    ~f:(fun line -> Sequence.exists (offsets_of line) ~f:is_code)
;;

let bodies_of comments =
  List.map comments ~f:(fun (comment : Comment.t) -> comment.extent.start, comment.body)
  |> Int.Map.of_alist_reduce ~f:Fn.const
;;

let create ~path ~comments text =
  let line_starts = line_starts_of text in
  let code_lines = code_lines_of text line_starts comments in
  { path; text; line_starts; code_lines; comments = bodies_of comments }
;;

(* The last line start is the length of the text, which starts no line. *)
let line_at source offset =
  let lines = Array.length source.code_lines in
  let order = `Last_less_than_or_equal_to in
  match
    Array.binary_search source.line_starts ~len:lines ~compare:Int.compare order offset
  with
  | Some line -> line
  | None -> 0
;;

let site_at source ~offset =
  let line = line_at source offset in
  let column = offset - source.line_starts.(line) in
  { Site.path = source.path; line = line + 1; column = column + 1 }
;;

let text_in source ({ start; stop } : Extent.t) =
  if 0 <= start && start <= stop && stop <= String.length source.text
  then Some (String.sub source.text ~pos:start ~len:(stop - start))
  else None
;;

let does_spell source ~offset spelling =
  offset >= 0
  && offset + String.length spelling <= String.length source.text
  && String.is_substring_at source.text ~pos:offset ~substring:spelling
;;

let code_lines_in source ({ start; stop } : Extent.t) =
  let first = line_at source start in
  let last = line_at source (Int.max start (stop - 1)) in
  Array.count (Array.sub source.code_lines ~pos:first ~len:(last - first + 1)) ~f:Fn.id
;;

let comment_after source ~offset =
  let is_blank letter = Char.equal letter ' ' || Char.equal letter '\t' in
  let%bind.Option opening =
    String.lfindi source.text ~pos:offset ~f:(fun _ letter -> not (is_blank letter))
  in
  Map.find source.comments opening
;;
