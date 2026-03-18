exception Invalid_string

module String = struct

include String

(* begins_with : string -> string -> bool *)
let begins_with s frag =
  let lf = length frag in
  if length s < lf then false else sub s 0 lf = frag

(* field : string -> char -> int -> string*)
(** Returns the Pick-like dynamic array element in position pos *)
let field s cDelim pos =
  let len = length s in
  let rec findStart fromPos cnt =
    (* first case also catches pos = 0 *)
    if cnt = pos then fromPos
    else if cnt > pos || fromPos > len then -1
    else
      let nextPos =
        try index_from s fromPos cDelim
        with Not_found -> len + 1
      in
      if nextPos > len then -1
      else findStart (nextPos + 1) (cnt + 1)
  in
  let startPos = findStart 0 0 in
  if startPos < 0 then ""
  else begin
    let endPos =
      try index_from s startPos cDelim
      with Not_found -> len
    in
    sub s startPos (endPos - startPos)
  end

(** filter string through user-defined function. The function takes a
    single character argument and returns a string. *)
let filter f s =
  let len = length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    Buffer.add_string buf (f s.[i])
  done;
  Buffer.contents buf

(* chop 1 char off left or right of string *)
let lchop s = if s = "" then "" else sub s 1 (length s - 1)
let rchop s = if s = "" then "" else sub s 0 (length s - 1)

(* listCat : string list -> string -> string *)
(* inverse of nsplit if sDelim is only 1 character *)
(** returns a string with the list elements delimited by sDelim *)
let listCat lst sDelim =
  let ad x y = x ^ sDelim ^ y in
  let s = List.fold_left ad "" lst in
  sub s 1 (length s - 1)


(** Split string into a list.
  nsplit "a,b,c" ',' -> ["a"; "b"; "c"]
  nsplit "a,b,c," ',' -> ["a"; "b"; "c"; ""]
  nsplit "," ',' -> [""; ""]
*)
let nsplit inStr cDelim =
  let s = Buffer.create 80
  and len = String.length inStr
  and lst = ref []
  in
  for i = 0 to len - 1 do
    let c = inStr.[i] in
    if c = cDelim then begin
      lst := Buffer.contents s :: !lst;
      Buffer.clear s;
      if i = len - 1 then lst := "" :: !lst
    end else
      Buffer.add_char s c
  done;
  if Buffer.length s > 0 then lst := Buffer.contents s :: !lst;
  List.rev !lst

(* replace s sa sb searches for all occurrences of string sa in
   string s and replaces them with string sb.
   replace "abcababcaabc" "abc" "aabc" will return "aabcabaabcaaabc" *)
let replace s a b =
  let alen = length a in
  let slen = length s in
  if alen < 1 || slen < 1 then
    s
  else begin
    let newstr = Buffer.create slen in
    (* sp indicates where the current matching began.  sap indicates the
     * current position of string a that is being checked for a match. *)
    let rec loop sp sap =
      if sap = alen then begin
        (* We have a match *)
        Buffer.add_string newstr b;
        loop (sp + sap) 0
      end else if sp + sap >= slen then
        Buffer.add_string newstr (sub s sp sap)
      else if s.[sp + sap] = a.[sap] then
        loop sp (sap + 1)
      else begin
        Buffer.add_string newstr (sub s sp 1);
        loop (sp + 1) 0
      end
    in
    loop 0 0;
    Buffer.contents newstr
  end


(* slice : string -> int -> int -> string *)
let slice istr ifirst ilast =
(** slice will return a sub string of istr beginning at first and
  ending at the character just before last.  If last is 0, then the
  sub will go to the end of the string.  Negative numbers count from
  the end of the string.  Any numbers greater than the length of the
  string are truncated. *)
  let len = length istr in
  let first = max 0 (min len (if ifirst < 0 then len + ifirst else ifirst)) in
  let last = max first (min len (if ilast <= 0 then len + ilast else ilast)) in
  sub istr first (last - first)

(* split1 : string -> char -> (string, string) *)
let split1 s cDelim =
  try
    let pos = index s cDelim in
    let p1 = pos + 1 in
    (sub s 0 pos, sub s p1 (length s - p1))
  with exn -> raise Invalid_string

(** remove leading characters indicated by trash *)
let rec ltrim s trash =
  if s = "" || not (contains trash s.[0]) then s
  else ltrim (sub s 1 (length s - 1)) trash

(** remove trailing characters indicated by trash *)
let rec rtrim s trash =
  let l = length s in
  if l = 0 || not (contains trash s.[l - 1]) then s
  else rtrim (sub s 0 (l - 1)) trash

(* Needed for ocaml 3.12 *)
(** remove leading and trailing spaces *)
let trim ?(trash=" ") s = rtrim (ltrim s trash) trash

end

module List = struct

include List

(** Needed for ocaml 3.12 *)
let iteri (f : int -> 'a -> unit) lst =
  let rec loop i lst =
    match lst with
    | [] -> ()
    | hd :: tl -> f i hd; loop (i + 1) tl
  in
  loop 0 lst  

(** return lst without the nth element *)
let remove lst n =
  let rec loop pos oldlist newlist =
    match oldlist with
    | hd :: tl ->
      if pos = n then newlist @ tl
      else loop (pos + 1) tl (newlist @ [hd])
    | [] -> newlist
  in
  loop 0 lst []

end
