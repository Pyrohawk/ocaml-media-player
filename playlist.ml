(* 2007-10-20 *)

open Qlib
open G
open Tk

let playQueue = ref []
let dirtyQueue = ref false
let playHist = Array.make 30 0
let histCnt = ref 0

let searchPattern = ref ""
let searchPos = ref 0

(* Use an array for quick access and typo safety. *)
let columns = [| "tvid"; "tvname"; "tvrating"; "tvlastplayed"; "tvlists";
  "tvtimesplayed"; "tvflags" |]
let tvid = 0
let tvname = 1
let tvrating = 2
let tvlastplayed = 3
let tvlists = 4
let tvtimesplayed = 5
let tvflags = 6

let headings = [| "Id"; "File Name"; "Rate"; "Last Play"; "Lists"; "Cnt";
  "Flags" |]

let getExtension f =
  let base = try Filename.chop_extension f with exn -> f in
  let extLength = String.length f - String.length base in
  if extLength > 0 then
    String.lowercase_ascii (String.slice f (0 - extLength) 0)
  else
    ""

let pushHist n =
  if !histCnt > 28 then (
    for i = 0 to 28 do playHist.(i) <- playHist.(i + 1) done;
    decr histCnt
  );
  playHist.(!histCnt) <- n;
  incr histCnt

let popHist () =
  if !histCnt > 0 then (
    decr histCnt;
    playHist.(!histCnt)
  ) else (-1)

type entryType = { mutable fn : string; mutable rating : int;
	mutable last : float; mutable lists : string; mutable cnt : int;
	mutable flags : string }

let _MEMDEBUG = false
let _KEYDEBUG = false

let formatEntry entry cnt ?(nw = max_int) scroll =
  let len = String.length entry.fn in
  let fn =
    if len > nw then (String.sub entry.fn (len - nw) nw) else entry.fn
  in
(*  let len = String.length fn in
  let fn = fn ^ (if len < nw then String.make (nw - len) ' ' else "") in
*)
  let fmtime =
    if scroll && entry.last = 0.0 then
      "(First play)"
    else
      let ltm = Unix.localtime entry.last in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d"
        (ltm.Unix.tm_year + 1900) (ltm.Unix.tm_mon + 1) ltm.Unix.tm_mday
        ltm.Unix.tm_hour ltm.Unix.tm_min
  in
  [Printf.sprintf "%06d:" cnt; fn; Printf.sprintf "%3d%%" entry.rating;
   fmtime; entry.lists; Printf.sprintf "%dx" entry.cnt; entry.flags]

(* Used to use DynArray for this, but we never do inserts, so a simple
   hashtable will work. *)
class ['a(*'*)]  cDynArray name = object(self)
  val mutable len = 0
  val mutable arr = Hashtbl.create 11

  method private index_err who ndx =
    failwith (Printf.sprintf "%s#get: index=%d, size=%d" name ndx len)

  method iter (f : 'a(*'*) -> unit) =
    for i = 0 to len - 1 do f (Hashtbl.find arr i) done
    
  method iteri (f : int -> 'a(*'*) -> unit) =
    for i = 0 to len - 1 do f i (Hashtbl.find arr i) done

  method get ndx =
    if ndx < 0 || ndx >= len then self#index_err "get" ndx;      
    Hashtbl.find arr ndx

  method set ndx (entry : 'a(*'*)) =
    if ndx < 0 || ndx >= len then self#index_err "set" ndx;
    Hashtbl.replace arr ndx entry

  method length = len

  method add (entry : 'a(*'*)) =
    Hashtbl.replace arr len entry;
    len <- len + 1
  
  method to_array =
    if len = 0 then self#index_err "to_array" 0;
    let a = Array.make len (Hashtbl.find arr 0) in
    for i = 0 to len - 1 do
      a.(i) <- Hashtbl.find arr i
    done;
    a

  method of_list lst =
    Hashtbl.clear arr;
    len <- 0;
    let rec loop l = match l with
    | h :: t -> self#add h; loop t
    | [] -> ()
    in
    loop lst

end

let playlist = new cDynArray "playlist"

let pushQueue n =
  if n < playlist#length && not (List.mem n !playQueue) then (
    playQueue := [n] @ !playQueue;
  )

(* 2011-02-23 - Update Listbox from omp main loop *)
let listActive = ref false
let updateListQueue = Queue.create ()

(* following is  never executed.  It's there so OCaml can see what
   type playlist is *)
let dummy () =
  let entry = { fn=""; rating=0; last=0.0; lists=""; cnt=0; flags=""} in
  playlist#add entry;
  Queue.add (0, entry) updateListQueue

let window = Tk.opentk ()

let heapCheck msg =
  if _MEMDEBUG then (
    let s = Gc.quick_stat () in
    Printf.printf "%s Current/Max Heap size = %d/%d\n"
      msg (s.Gc.heap_words * 4) (s.Gc.top_heap_words * 4)
  )

let fileType s =
  (* Only concerned with regular files and directories at this time
     Return type and directory contents if directory *)
  try
    let uStat = Unix.lstat s in
    match uStat.Unix.st_kind with
      Unix.S_REG -> ("R", Array.make 1 "")
    | Unix.S_DIR -> let a = Sys.readdir s in Array.sort compare a; ("D", a)
    | _ -> ("O", Array.make 1 "")
  with exn -> ("x", Array.make 1 "")

let fileSize s =
  try
    let uStat = Unix.lstat s in
    uStat.Unix.st_size
  with exn -> 0

let msg message mType =
  let title = if mType = "E" then "Error" else "Message" in
  let buttons = ["OK"] in
  let _ = Dialog.create ~parent:window ~title ~message ~buttons () in
  prerr_endline message

let getString text default =
  let parent = Toplevel.create window in
  Wm.title_set parent "omp User Entry";
  let frame = Ttk_frame.create parent in
  let lbl = Ttk_label.create ~text frame in
  Tk.pack [lbl];
  let tv = Textvariable.create () in
  let txt = Ttk_entry.create ~textvariable:tv frame in
  Tk.pack [txt];
  Textvariable.set tv default;
  let enter ev =
    Tk.destroy parent
  in
  let escape ev =
    Textvariable.set tv default;
    Tk.destroy parent
  in
  Tk.bind ~events:[`KeyPressDetail "Return"] ~action:(fun ev -> enter ev)
    parent;
  Tk.bind ~events:[`KeyPressDetail "Escape"] ~action:(fun ev -> escape ev)
    parent;
  Tk.pack [frame];
  Focus.set txt;
  Ttk_entry.selection_range ~start:(`Num 0) ~stop: `End txt;
  Tkwait.window parent;
  Textvariable.get tv

let updateArray which entry =
  playlist#set which entry

let edit () =
  let selectionSave = ref [] in
  
  let cleartree tree =
    Listbox.delete ~first:(`Num 0) ~last:`End tree
  in

  let font = Font.create ~family:!userfont ~size:!fontSize
  	~weight:!fontWeight () in
  
  let histPos = ref (max 0 (!histCnt - 1)) in

  let w = Toplevel.create window in
  Wm.title_set w "omp Playlist Editor";
  let frame = Ttk_frame.create w in
  let listFrame = Ttk_frame.create frame in
  let yscroll = Ttk_scrollbar.create ~orient:`Vertical listFrame in
  let xscroll = Ttk_scrollbar.create ~orient:`Horizontal listFrame in
  let lstBx = Listbox.create 
        ~font
        ~height:30
        ~selectmode:`Extended
        ~yscrollcommand:(Scrollbar.set yscroll)
        ~xscrollcommand:(Scrollbar.set xscroll) listFrame
  in  
  Scrollbar.configure ~command:(Listbox.yview lstBx) yscroll;
  Scrollbar.configure ~command:(Listbox.xview lstBx) xscroll;

  Tk.pack ~side:`Right ~fill:`Y [yscroll];
  Tk.pack ~side:`Bottom ~fill:`X [xscroll];
  Tk.pack ~expand:true ~side:`Left ~fill:`Both [lstBx];
  Tk.pack ~expand:true ~fill:`Both [listFrame];

  (* ----------------- Queue window -------------------- *)
  let nokill () =
    (* Don't allow qw to be killed by the user *)
    ()
  in
  let qw = Toplevel.create window in
  Wm.title_set qw "Queue Editor";
  Wm.protocol_set ~name: "WM_DELETE_WINDOW" ~command:nokill qw;
  (*Wm.iconify qw;*)
  let qframe = Ttk_frame.create qw in
  let qys = Ttk_scrollbar.create ~orient:`Vertical qframe in
  let qxs = Ttk_scrollbar.create ~orient:`Horizontal qframe in
  let qlb = Listbox.create 
        ~font
        ~height:10 
        ~selectmode:`Browse
        ~yscrollcommand:(Scrollbar.set qys)
        ~xscrollcommand:(Scrollbar.set qxs) qframe in
  Scrollbar.configure ~command:(Listbox.yview qlb) qys;
  Scrollbar.configure ~command:(Listbox.xview qlb) qxs;
  Tk.pack ~side:`Right ~fill:`Y [qys];
  Tk.pack ~side:`Bottom ~fill:`X [qxs];
  Tk.pack ~expand:true ~side:`Left ~fill:`Both [qlb];
  Tk.pack ~expand:true ~fill:`Both [qframe];

  (* 2008-02-09 Added the range check in case someone sets the last song
     in the list to link with the next song *)
  let qInsert pos n =
    let entry = playlist#get n in
    let disp_id = Printf.sprintf "%04d:" n in
    let row_text = disp_id ^ " " ^ entry.fn in
    Listbox.insert ~index:pos ~texts:[row_text] qlb
  in
  let fillQueue () =
    cleartree qlb;
    List.iter (qInsert `End) !playQueue;
  in
  fillQueue ();
  let addQueue n =
    if n < playlist#length then (
      playQueue := !playQueue @ [n];
      qInsert `End n
    )
  in
  let qGetSelection () =
    let selected = Listbox.curselection qlb in
    if selected = [] then (
      msg "No selection!" "E";
      -1
    ) else (
      match List.hd selected with
      | `Num i -> List.nth !playQueue i
      | _ -> -1
    )
  in
  let delQueue () =
    let ndx = qGetSelection () in
    if ndx >= 0 then (
      playQueue := List.remove !playQueue ndx;
      fillQueue ()
    )
  in
  let moveQueue where =
    (* up = true for up, false for down *)
    let select = qGetSelection () in
    let ndx = listSearch !playQueue select in
    let len = List.length !playQueue in
    let a = Array.of_list !playQueue in
    if (ndx > 0 && (where = "u" || where = "t")) ||
       (ndx < len - 1 && (where = "d" || where = "b")) then (
      let save = a.(ndx) in
      let target = ref 0 in
      if where = "u" || where = "d" then (
        target := if where = "u" then ndx - 1 else ndx + 1;
        a.(ndx) <- a.(!target);
        a.(!target) <- save;
      ) else if where = "t" then (
        for i = ndx downto 1 do
	  a.(i) <- a.(i - 1)
	done;
	a.(0) <- save
      ) else if where = "b" then (
        target := len - 1;
        for i = ndx to len - 2 do
	  a.(i) <- a.(i + 1)
	done;
	a.(!target) <- save
      );
      playQueue := Array.to_list a;
      fillQueue ();
      Listbox.activate ~index:(`Num !target) qlb;
      Listbox.see ~index:(`Num !target) qlb;
      Focus.set qlb;
    )
  in
  let upQueue () = moveQueue "u" in
  let downQueue () = moveQueue "d" in
  let topQueue () = moveQueue "t" in
  let bottomQueue () = moveQueue "b" in
  let qpopup = Menu.create ~typ:`Normal frame in
  Menu.add_command ~label:"Refresh (r)" ~command:fillQueue qpopup;
  Menu.add_command ~label:"Delete (Delete)" ~command:delQueue qpopup;
  Menu.add_command ~label:"Move up (u)" ~command:upQueue qpopup;
  Menu.add_command ~label:"Move down (d)" ~command:downQueue qpopup;
  Menu.add_command ~label:"Move to top (t)" ~command:topQueue qpopup;
  Menu.add_command ~label:"Move to bottom (b)" ~command:bottomQueue qpopup;
  let doQPopup evt =
    Menu.popup ~x:evt.Tk.ev_RootX ~y:evt.Tk.ev_RootY qpopup;
  in
  let qKeyPress e =
    let sym = e.Tk.ev_KeySymString
(*    and state = (int_of_string e.Tk.ev_State) mod 16 *)
    in
    if sym = "Menu" || sym = "F2" then doQPopup e
    else if sym = "Delete" then delQueue ()
    else if sym = "r" then fillQueue ()
    else if sym = "u" || sym = "d" || sym = "t" || sym = "b" then moveQueue sym
  in
  Tk.bind ~events:[`ButtonPressDetail 3] ~fields:[`RootX; `RootY]
  	~action:(fun ev -> doQPopup ev) qw;
  Tk.bind ~events:[`KeyPress] ~fields:[`State; `KeySymString; `Char]
  	~action:(fun ev -> qKeyPress ev) qw;
  let rec pollQueue () =
    if !dirtyQueue then (
      dirtyQueue := false;
      fillQueue ()
    );
    Timer.set ~ms:1000 ~callback:pollQueue
  in
  pollQueue ();

  (* ---------------------- End Queue ----------------------- *)

  (* Call with List.iteri (addField id) (formatEntry ...) *)
  let addEntry n entry =
    let fields = formatEntry entry n false in
    let row_text = String.concat "  " fields in
    Listbox.insert ~index:`End ~texts:[row_text] lstBx
  in

  let fillList () =
    playlist#iteri addEntry;
  in
  fillList ();

  let chooseDir initialdir =
    Protocol.tkEval [|
      Protocol.TkToken "tk_chooseDirectory";
      Protocol.TkToken "-parent"; Protocol.TkToken (Widget.name w);
      Protocol.TkToken "-title"; Protocol.TkToken "Choose directory";
      Protocol.TkToken "-initialdir"; Protocol.TkToken initialdir;
      Protocol.TkToken "-mustexist"; Protocol.TkToken "0"
    |]
  in

  let updateListbox which entry =
    updateArray which entry;
    let fields = formatEntry entry which false in
    let row_text = String.concat "  " fields in
    Listbox.delete ~first:(`Num which) ~last:(`Num which) lstBx;
    Listbox.insert ~index:(`Num which) ~texts:[row_text] lstBx
  in

  let seeLast id_str =
    let idx = `Num (int_of_string id_str) in
    Listbox.see ~index:idx lstBx;
    Listbox.activate ~index:idx lstBx;
    Focus.set lstBx;
    Tk.update ()
  in

  let clearSelect id_str =
    Listbox.selection_clear ~first:(`Num 0) ~last:`End lstBx;
    Listbox.selection_set ~first:(`Num (int_of_string id_str)) ~last:(`Num (int_of_string id_str)) lstBx;
    seeLast id_str
  in

  let refreshList id =
    cleartree lstBx;
    fillList ();
    seeLast id
  in

  let rec search file cnt =
    if cnt >= playlist#length then true
    else (
      let entry = playlist#get cnt in
      if entry.fn = file then false else search file (cnt + 1)
    )
  in

  let addFile () =
(*    let e = List.map (fun a -> " " ^ a) !extensions in*)
    let e = !extensions in
    let filetypes = [{Tk.typename="Multimedia Files"; extensions=e;
      mactypes = []};
      {Tk.typename = "All Files"; extensions = ["*"]; mactypes = []}]
    in
(*  let filetypes = [("Multimedia Files", e, []);
      ("All Files", ["*"], [])]
    in
*)
    let selectFile = Tk.getOpenFile ~filetypes
      ~initialdir:!previousPath ~parent:w ~title:"Add File" ()
    in
    if selectFile <> "" && List.mem (getExtension selectFile) !extensions then (
      previousPath := Filename.dirname selectFile;
      if search selectFile 0 then (
        let entry = { fn = selectFile; rating = !defaultScore;
            last = 0.0; lists = "A"; cnt = 0; flags = "" }
        in
        playlist#add entry;
	msg ("Added: " ^ selectFile) "";
	let n = playlist#length - 1 in
	addEntry n entry;
        seeLast (string_of_int (playlist#length - 1))
      ) else
        msg ("Duplicate: " ^ selectFile) "";
    )
  in
  let addDir () =
    let curDir = Sys.getcwd () in
    let selectDir = chooseDir curDir in
    if selectDir <> "" then (
      let numAdded = ref 0 in
      let parent = Toplevel.create window in
      let frame = Ttk_frame.create parent in
      let scroll = Scrollbar.create frame in
      let txt = Text.create ~height:20 ~takefocus:false ~width:80 frame in
      Text.configure ~yscrollcommand:(Scrollbar.set scroll) txt;
      Scrollbar.configure ~command:(Text.yview txt) scroll;
      Tk.pack ~side:`Left ~fill:`Both [txt];
      Tk.pack ~side:`Right ~fill:`Y [scroll];
      Tk.pack [frame];
      let addText s =
        let text = s ^ "\n" in
        Text.insert ~index:(`End,[]) ~text txt;
        Text.see ~index:(`End,[]) txt;
      in
      let addThisFile f =
        (* Check for duplicate.  It might be faster to form a hash
         * table or some quick lookup method to avoid searching the
         * entire array each time.  Unfortunately, since we can't
         * keep the array sorted, we can't do a binary search.
         * However, that would also take up more memory.  It runs
         * plenty fast on my machines but not sure what it
	 * would do on slower ones.
         *)
	let f1 =
          if String.begins_with f "//" then String.lchop f else f
        in
        if search f1 0 then (
          playlist#add
            { fn = f1; rating = !defaultScore; last = 0.0;
                lists = "A"; cnt = 0; flags = "" };
	  incr numAdded;
          addText ("Added " ^ f1);
        )
      in
      let rec addThisDir d first =
        let (typ, contents) = fileType d in
        match typ with
          "D" ->
	    addText ("Searching " ^ d);
	    let len = Array.length contents in
	    for i = 0 to len - 1 do
	      addThisDir (d ^ "/" ^ contents.(i)) false
	    done;
	    if first then
	      addText (Printf.sprintf
                "Done.  Added %d files." !numAdded)
	    else
	      Tk.update ()
        | "R" ->
	    if List.mem (getExtension d) !extensions then
              addThisFile d
        | _ -> ()
      in
      let addDirs () =
        addThisDir selectDir true
      in
      Timer.set ~ms:100 ~callback:addDirs;
      Tkwait.window parent;
      refreshList "0";
    );
  in
  let import () =
    let vuxDir =
      try
        (Sys.getenv "HOME") ^ "/.vux"
      with _ -> ".vux"
    in
    let fileName = Tk.getOpenFile ~initialdir:vuxDir
      ~initialfile:"scorelist" ~parent:w ~title:"Select vux scorelist file" ()
    in
    if fileName <> "" then (
      let ageTbl = Hashtbl.create 11 in
      let ageName = (String.slice fileName 0 (-9)) ^ "agelist" in
      if Sys.file_exists ageName then (
        let ageFile = open_in ageName in
	let rec readAge () =
	  try
	    let line = input_line ageFile in
	    let parse = String.nsplit line '"' in
	    let file = List.nth parse 1 in
	    let time = float_of_string (String.lchop (List.nth parse 2)) in
	    Hashtbl.add ageTbl file time;
	    readAge ()
	  with _ -> ()
	in
	readAge ();
	close_in ageFile;
      );
      let inFile = open_in fileName in
      let rec loop () =
        try
          let line = input_line inFile in
	  let parse = String.nsplit line '"' in
	  let file = List.nth parse 1 in
	  let rate = int_of_string (String.lchop (List.nth parse 2)) in
	  let lp =
	    if Hashtbl.mem ageTbl file then
	      Hashtbl.find ageTbl file
	    else 0.0
	  in
	  let entry =
	    { fn = file; rating = rate; last = lp; lists = "A"; cnt = 0;
	      flags = "" }
	  in
	  playlist#add entry;
	  let id = playlist#length - 1 in
	  addEntry id entry;
	  loop ()
	with _ -> ()
      in
      loop ()
    )
  in
  let importm3u () =
    let fileName =
      Tk.getOpenFile ~parent:w ~title:"Select .m3u playlist file" ()
    in
    if fileName <> "" then (
      let inFile = open_in fileName in
      let rec loop () =
        try
          let line = input_line inFile in
	  if line <> "" then (
	    let ch1 = String.sub line 0 1 in
	    if ch1 <> "#" && search line 0 then (	      
	      let entry =
	        { fn = line; rating = !defaultScore; last = 0.0;
                    lists = "A"; cnt = 0; flags = "" }
	      in
	      playlist#add entry;
	      let id = playlist#length - 1 in
	      addEntry id entry;
              Listbox.see ~index:(`Num id) lstBx;
	      Tk.update ();
	    );
	  );
	  loop ()
	with _ -> ()
      in
      loop ()
    )
  in
  let rec dupes () =
    (* First, we get the file sizes, which is fairly quick.  Then sort
       by size.  For matching sizes, get the md5sums, sort by them,
       and mark duplicates *)
    let sizes = Array.make playlist#length "" in
    let sdw = Toplevel.create window in
    let sdf = Ttk_frame.create sdw in
    let sdl = Ttk_label.create ~text:"Get entry file sizes" sdf in
    let tvDupeProgress = Textvariable.create () in
    Textvariable.set tvDupeProgress "0.0";
    Wm.title_set sdw "Search for Duplicates";
    let sdpb = Label.create ~textvariable:tvDupeProgress ~width:20 sdf
    in
    Tk.pack [sdl];
    Tk.pack [sdpb];
    Tk.pack [sdf];
    let flen = float playlist#length in
   
    for i = 0 to playlist#length - 1 do
      (*Ttk_treeview.see lstBx (string_of_int i);
      Tk.update ();*)
      let v = float i *. 100. /. flen in
      Textvariable.set tvDupeProgress (Printf.sprintf "%f" v);
      Tk.update ();
      let entry = playlist#get i in
      (* Make sure no old '=' flags are in the lists *)
      if String.contains entry.lists '=' then (
        let rep c =
	  match c with
	    '=' -> ""
	  | '0' .. '9' -> ""
          | _ -> String.make 1 c
	in
	playlist#set i
	  { entry with lists = String.filter rep entry.lists }
      );
      let size = fileSize entry.fn in
      sizes.(i) <- Printf.sprintf "%010d%d" size i;
    done;
    Array.sort compare sizes;
    let empty = String.make 10 '0'
    and prevSize = ref ""
    and prevId = ref (-10)
    and calcPrev = ref true
    and dsums = new cDynArray "dsums"
    and problems = ref []
    and matchNum = ref 0
    in
    (* problems.(n) = (matchNum (0 for missing or 0 size or 0 md5sum),
        id, matching_id list)
     *)
    Ttk_label.configure sdl ~text:"Get md5sums";
    Textvariable.set tvDupeProgress "0.0";
    for i = 0 to playlist#length - 1 do
      let size = String.sub sizes.(i) 0 10 in
      let sId = String.slice sizes.(i) 10 0 in
      let id = int_of_string sId in
      let v = float i *. 100. /. flen in
      Textvariable.set tvDupeProgress (Printf.sprintf "%f" v);
      (*Ttk_treeview.see lstBx sId;*)
      Tk.update ();
      if size = empty then (
        problems := (0, id, -10) :: !problems;
      ) else if size = !prevSize then (
        if !calcPrev then (
	  let entry = playlist#get !prevId in
	  let md5sum = Digest.file entry.fn in
          dsums#add (md5sum ^ (string_of_int !prevId));
	  calcPrev := false;
	);
        let entry = playlist#get id in
	let md5sum = Digest.file entry.fn in
        dsums#add (md5sum ^ sId);
      ) else calcPrev := true;
      prevSize := size;
      prevId := id;
    done;
    (* dsums now has list of files with the same sizes.  Sort by md5 *)
    if dsums#length > 0 then begin
      let sums = dsums#to_array in
      Array.sort compare sums;
      let prevSum = ref ""
      and prevAdded = ref false
      in
      let empty = String.make 16 '\000' in
      for i = 0 to Array.length sums - 1 do
        let md5sum = String.sub sums.(i) 0 16 in
        let id = int_of_string (String.slice sums.(i) 16 0) in
        if md5sum = empty then
          problems := (0, id, (-5)) :: !problems
        else if i > 0 && md5sum = !prevSum then (
          incr matchNum;
          if not !prevAdded then
            problems := (!matchNum, !prevId, id) :: !problems;
          problems := (!matchNum, id, !prevId) :: !problems;
          prevAdded := true
        ) else prevAdded := false;
        prevSum := md5sum;
        prevId := id;
      done;
    end;
    (*refreshList "0";*)
    Tk.destroy sdw;
    if List.length !problems > 0 then begin
      let ap = Array.of_list !problems in
      let cmp (a, aid, _) (b, bid, _) =
        if a < b then -1
        else if a > b then 1
        else if aid < bid then -1
        else if aid > bid then 1
        else 0
      in
      Array.sort cmp ap;
      showDupes ap
    end else
      msg "No duplicates found" ""
  and showDupes problems =
    let dw = Toplevel.create window in
    Wm.title_set dw "omp Problem Files";
    let dframe = Ttk_frame.create dw in
    let dys = Ttk_scrollbar.create ~orient:`Vertical dframe
    and dxs = Ttk_scrollbar.create ~orient:`Horizontal dframe
    in
    let height = min 30 (Array.length problems) in
    let dtree = Listbox.create ~font ~height ~selectmode:`Browse
        ~yscrollcommand:(Scrollbar.set dys)
        ~xscrollcommand:(Scrollbar.set dxs) dframe
    in
    Scrollbar.configure ~command:(Listbox.yview dtree) dys;
    Scrollbar.configure ~command:(Listbox.xview dtree) dxs;
    Tk.pack ~side:`Right ~fill:`Y [dys];
    Tk.pack ~side:`Bottom ~fill:`X [dxs];
    Tk.pack ~expand:true ~side:`Left ~fill:`Both [dtree];
    Tk.pack ~expand:true ~fill:`Both [dframe];

    let dInsert ndx (matchNum, id, matchId) =
      let entry = playlist#get id in
      let disp_id = Printf.sprintf "%04d:" id in
      let (problem, mtch) = match matchId with
      | (-10) -> ("Missing", "")
      | (-5) -> ("No Read", "")
      | _ -> ("Duplicate", string_of_int matchId)
      in
      let row_text = Printf.sprintf "%s  %-15s  %-6s  %s" disp_id problem mtch entry.fn in
      Listbox.insert ~index:`End ~texts:[row_text] dtree
    in
    Array.iteri dInsert problems;
    let dGetSelection () =
      let selected = Listbox.curselection dtree in
      if selected = [] then -1
      else match List.hd selected with
      | `Num i -> 
          let (_, id, _) = problems.(i) in 
          id
      | _ -> -1
    in

    let dChgSelect ev =
      let ndx = dGetSelection () in
      if ndx >= 0 then (
        let (_, id, _) = problems.(ndx) in
	Listbox.see ~index:(`Num id) lstBx;
        Tk.update ()
      )
    in

    let fixDupe cmd =
      let ndx = dGetSelection () in
      if ndx >= 0 then (
        let (mnum, id, mtch) = problems.(ndx) in
        if mtch > (-20) then begin
          let entry = playlist#get id in
          entry.lists <- "x";
          playlist#set id entry;
          let (status, disp) =
            if cmd = 'd' then (-99, "Deleted") else (-20, "Removed")
          in
          problems.(ndx) <- (mnum, id, status);
          let (_, id, matchId) = problems.(ndx) in
          let entry = playlist#get id in
          let disp_id = Printf.sprintf "%04d:" id in
          let mtch = if matchId < 0 then "" else string_of_int matchId in
          let row_text = Printf.sprintf "%s  %-15s  %-6s  %s" disp_id disp mtch entry.fn in
          Listbox.delete ~first:(`Num ndx) ~last:(`Num ndx) dtree;
          Listbox.insert ~index:(`Num ndx) ~texts:[row_text] dtree;
          updateListbox id entry;
          if cmd  = 'd' then begin
            try
              Sys.remove entry.fn
            with _ -> msg "Unable to remove file" "E"
          end
        end
      )
    in

    let dupe_remove () = fixDupe 'r' in

    let dupe_delete () =
      let resp = getString
        "Enter 'yes' to delete this file" "no"
      in
      if resp = "yes" then fixDupe 'd'
    in
      
    let dupe_escape () =
      Tk.destroy dw
    in

    let dpopup = Menu.create ~typ:`Normal dframe in
    let addpop label command =
      Menu.add_command ~label ~command dpopup
    in
    addpop "Remove from Playlist (r)" dupe_remove;
    addpop "Delete file (Delete)" dupe_delete;
    let doDPopup evt =
      Menu.popup ~x:evt.Tk.ev_RootX ~y:evt.Tk.ev_RootY dpopup
    in

    let dKeyPress e =
      let sym = e.Tk.ev_KeySymString in
      if sym = "Menu" || sym = "F2" then doDPopup e
      else if sym = "Delete" then dupe_delete ()
      else if sym = "r" then dupe_remove ()
      else if sym = "Escape" then dupe_escape ()
    in
    Tk.bind ~events:[`ButtonPressDetail 3] ~fields:[`RootX; `RootY]
      ~action:(fun ev -> doDPopup ev) dw;
    Tk.bind ~events:[`KeyPress] ~fields:[`KeySymString]
      ~action:(fun ev -> dKeyPress ev) dw;
    Tk.bind ~events:[`Virtual "TreeviewSelect"] ~action:dChgSelect dw;

    Tkwait.window dw     
  in

  let rec processUpdateList () =
    while not (Queue.is_empty updateListQueue) do
      let (which, entry) = Queue.take updateListQueue in
      updateListbox which entry
    done;
    if !listActive then Timer.set ~ms:500 ~callback:processUpdateList
  in

  let quit () =
    listActive := false;
    (* Clear any updates pending *)
    processUpdateList ();
    Tk.destroy qw;
    Tk.destroy w
  in  

  (* Popup menu *)
  let getListName () =
    let resp = getString "Enter 1 letter list name(s):" "" in
    let rec strip str =
      if String.contains str ' ' then
        strip (String.replace str " " "")
      else str
    in
    strip resp
  in
  (* nmax = maximum selection size *)
  let saveSelect limit =
    selectionSave := List.fold_right (fun idx acc -> 
      match idx with `Num i -> string_of_int i :: acc | _ -> acc
    ) (Listbox.curselection lstBx) [];
    let selectCnt = List.length !selectionSave in
    let selectCnt =
      if selectCnt < 1 then (
        msg "No items selected!" "E";
	0
      ) else if limit && selectCnt > 1 then (
        msg "One (1) and only one item may be selected." "E";
	0
      ) else selectCnt
    in
    selectCnt
  in
  let restoreSelect () =
    List.iter (fun id_str -> 
      let idx = `Num (int_of_string id_str) in
      Listbox.selection_set ~first:idx ~last:idx lstBx
    ) !selectionSave
  in
  let processSelection callback =
    let selected = 
      List.fold_right (fun idx acc -> 
        match idx with `Num i -> string_of_int i :: acc | _ -> acc
      ) (Listbox.curselection lstBx) [] 
    in
    let selectCnt = List.length selected in
    let lastSelect = ref "" in
    if selectCnt > 0 then (
      for i = 0 to selectCnt - 1 do
        let x = List.nth selected i in
        callback (int_of_string x);
	lastSelect := x
      done;
      clearSelect !lastSelect
    )
  in
  let queue () =
    processSelection addQueue
  in
  let pqueue () =
    processSelection pushQueue;
    fillQueue ()
  in
  let addList replaceFlag newList =
    if saveSelect false > 0 then (
      let nList = if newList = "" then getListName () else newList in
      if nList <> "" then (
        restoreSelect ();
        let addListCB x =
          let entry = playlist#get x in
          let withAdded = ref "" in
          withAdded := if replaceFlag then "" else entry.lists;
          for i = 0 to String.length nList - 1 do
	    let ch = String.sub nList i 1 in
	    withAdded := if String.contains !withAdded ch.[0] then
	      !withAdded else !withAdded ^ ch
          done;
	  entry.lists <- !withAdded;
	  updateListbox x entry
        in
        processSelection addListCB
      )
    )
  in    
  let copy () = addList false "" in
  let move () = addList true "" in
  let remove () = addList true "x" in
  let link () =
    let linkCB x =
      let entry = playlist#get x in
      entry.flags <- toggleLetter entry.flags "n";
      updateListbox x entry
    in
    processSelection linkCB
  in
  let lock () =
    let lockCB x =
      let entry = playlist#get x in
      entry.flags <- toggleLetter entry.flags "l";
      updateListbox x entry
    in
    processSelection lockCB
  in
  let selectClear () =
    (* This may look odd, but set clears all other selections *)
    Listbox.selection_clear ~first:(`Num 0) ~last:`End lstBx;
    Listbox.selection_set ~first:(`Num 0) ~last:(`Num 0) lstBx;
  in
  let selectList () =
    let sl = getString "Enter list id: " "" in
    if sl <> "" then (
      for i = 0 to playlist#length - 1 do
        let entry = playlist#get i in
	if String.contains entry.lists sl.[0] then
	  Listbox.selection_set ~first:(`Num i) ~last:(`Num i) lstBx
      done;
    );
  in
  let getRating () =
    let resp = getString "Enter new rating [+-](0-100)[%]:" "" in
    let len = String.length resp in
    let (m, v) =
      if len > 1 then (
        let ch1 = String.sub resp 0 1 in
	let chz = String.sub resp (len - 1) 1 in
	let (chz, resp) = if chz = "%" then (chz, rchop resp) else ("", resp) in
	let rest = lchop resp in
	if ch1 = "+" || ch1 = "-" then (ch1 ^ chz, ifInt rest)
	else (chz, ifInt resp)
      ) else ("", ifInt resp)
    in
    if v < 0 || v > 100 then ("", min_int) else (m, v)
  in
  let setRating () =
    if saveSelect false > 0 then (
      let (mods, newRating) = getRating () in
      restoreSelect ();
      if newRating >= 0 then (
        let pctFlag = String.contains mods '%' in
	let addFlag = String.contains mods '+' in
	let subFlag = String.contains mods '-' in
        let setRatingCB x =
	  let entry = playlist#get x in
	  entry.flags <- entry.flags ^
	    (if String.contains entry.flags 'l' then "" else "l");
          if pctFlag then (
	    let adj = entry.rating * newRating / 100 in
	    if subFlag then entry.rating <- entry.rating - adj
	    else entry.rating <- entry.rating + adj
	  ) else if addFlag then
	    entry.rating <- entry.rating + newRating
	  else if subFlag then
	    entry.rating <- entry.rating - newRating
	  else
	    entry.rating <- newRating;
	  if entry.rating < 0 then entry.rating <- 0;
	  if entry.rating > 100 then entry.rating <- 100;
	  updateListbox x entry
        in
        processSelection setRatingCB
      )
    )
  in

  let renameFile fileName newName ndx entry =
    if Sys.file_exists newName then
      msg "A file already exists by that name." "E"
    else (
      let good =
	try Sys.rename fileName newName; true
        with exn -> false
      in
      if good then begin
	entry.fn <- newName;
	updateListbox ndx entry
      end else
	msg (Printf.sprintf "Can't rename file %s to %s." fileName newName) "E"
    )
  in

  let rename () =
    let selected = 
      List.fold_right (fun idx acc -> 
        match idx with `Num i -> string_of_int i :: acc | _ -> acc
      ) (Listbox.curselection lstBx) [] 
    in
    if List.length selected <> 1 then (
      msg "One and only one item must be selected to rename!" "E";
    ) else (
      let id = List.nth selected 0 in
      let x = int_of_string id in
      let entry = playlist#get x in
      let fullname = entry.fn in
      let path = Filename.dirname fullname in
      let filename = Filename.basename fullname in
      let newname = getString "Enter new file name:" filename in
      if newname <> filename && newname <> "" then (
	let newfull = path ^ "/" ^ newname in
	renameFile fullname newfull x entry
      )
    )
  in

  (* This "feature" doesn't do anything presently.  The code is here because
   * I decided I wanted another feature and need to think about this one
   * some more. *)
  let normalize n = n    
  in

  let normName () =
    if saveSelect false > 0 then (
      restoreSelect ();
      let cbNormName f =
	let entry = playlist#get f in
	let fileName = Filename.basename entry.fn
	and dir = Filename.dirname entry.fn in
	let newName = normalize fileName in
	renameFile entry.fn (Filename.concat dir newName) f entry;
      in
      processSelection cbNormName
    )
  in

  let mv () =
    if saveSelect false > 0 then (
      let s1 = int_of_string (List.nth !selectionSave 0) in
      let entry = playlist#get s1 in
      let defaultDir = Filename.dirname entry.fn in
      let selectDir = chooseDir defaultDir in
      if selectDir <> "" then (
	let continue =
	  if Sys.file_exists selectDir then
	    true
	  else (
	    let cmd = "mkdir -p " ^ (Filename.quote selectDir) in
	    print_endline cmd;
	    let res = Sys.command cmd in
	    res = 0
	  )
        in
	restoreSelect ();
	let mvFile f =
	  let entry = playlist#get f in
	  let fileName = Filename.basename entry.fn in
	  let cmd = Printf.sprintf "mv %s %s" (Filename.quote entry.fn)
		(Filename.quote selectDir)
	  in
	  let res = Sys.command cmd in
	  if res = 0 then begin
	    entry.fn <- Filename.concat	selectDir fileName;
	    updateListbox f entry
	  end else
	    Printf.printf "Unable to move %s to %s." entry.fn selectDir
	in
	if continue then begin
	  processSelection mvFile
	end
      )
    )
  in

  let popup = Menu.create ~typ:`Normal frame in
  Menu.add_command ~label:"Copy to list (^c)" ~command:copy popup;
  Menu.add_command ~label:"Move to list (^m)" ~command:move popup;
  Menu.add_command ~label:"Move to trash (^x)" ~command:remove popup;
  Menu.add_command ~label:"Lock rating (^l)" ~command:lock popup;
  Menu.add_command ~label:"Link to next song (^n)" ~command:link popup;
  Menu.add_command ~label:"Clear Select" ~command:selectClear popup;
  Menu.add_command ~label:"Select by list" ~command:selectList popup;
  Menu.add_command ~label:"Set rating (^r)" ~command:setRating popup;
  Menu.add_command ~label:"Queue to play (^q)" ~command:queue popup;
  Menu.add_command ~label:"Push on top of queue to play (^p)" ~command:pqueue
  	popup;
  Menu.add_command ~label:"Rename file (^f)" ~command:rename popup;
  Menu.add_command ~label:"Move files (^d)" ~command:mv popup;
  let doPopup evt =
    Menu.popup ~x:evt.Tk.ev_RootX ~y:evt.Tk.ev_RootY popup;
  in

  let missing () =
    for i = 0 to playlist#length - 1 do
      if i mod 100 = 0 then (
        Listbox.see ~index:(`Num i) lstBx;
        Tk.update ();
      );
      let entry = playlist#get i in
      if not (String.contains entry.lists 'p') then (
        if not (Sys.file_exists entry.fn) then (
	  entry.lists <- "x";
	  playlist#set i entry
	)
      )
    done;
    refreshList "0"
  in

  let buttons = Ttk_frame.create frame in
  let btnAddFile = Ttk_button.create ~text:"Add File" ~command:addFile buttons in
  let btnAddDir = Ttk_button.create ~text:"Add Dir" ~command:addDir buttons in
  let btnAddM3U = Ttk_button.create ~text:"Add M3U" ~command:importm3u buttons in
  let btnImport = Ttk_button.create ~text:"Import vux" ~command:import buttons in
  let btnDupes = Ttk_button.create ~text:"Dupes" ~command:dupes buttons in
  let btnMissing = Ttk_button.create ~text:"Trash Missing" ~command:missing
  	buttons in
  let btnExit = Ttk_button.create ~text:"Close" ~command:quit buttons in
  Tk.pack ~side:`Left [btnAddFile; btnAddDir; btnAddM3U; btnImport;
    btnDupes; btnMissing; btnExit];
  Tk.pack ~side:`Bottom [buttons];
  Tk.pack ~expand:true ~fill:`Both [frame];

  let addBind e =
    Tk.bind ~events:[e] ~fields:[`RootX; `RootY]
      ~extend:true ~action:(fun ev -> doPopup ev) w;
  in
  addBind (`ButtonPressDetail 3);
  let searchNext forward =
    if forward then incr searchPos else decr searchPos;
    if String.length !searchPattern > 0 then (
      let (nocase, str) =
        if String.sub !searchPattern 0 1 = "!" then
	  (true, String.lowercase_ascii (String.slice !searchPattern 1 0))
	else
	  (false, !searchPattern)
      in
      let rec look i =
        if i < playlist#length && i >= 0 then (
          let srchEntry =
	    let entry = playlist#get i in
	    let fmt = formatEntry entry i ~nw:0 false in
	    (if nocase then String.lowercase_ascii entry.fn
	    else entry.fn) ^ (String.listCat fmt "|")
	  in
          if String.contains srchEntry str.[0] then (
	    Printf.printf "Found #%d  \n" i; flush stdout;
	    Listbox.see ~index:(`Num i) lstBx;
            Listbox.activate ~index:(`Num i) lstBx;
            Listbox.selection_clear ~first:(`Num 0) ~last:`End lstBx;
            Listbox.selection_set ~first:(`Num i) ~last:(`Num i) lstBx;
            Focus.set lstBx; 
            searchPos := i
	  ) else
	    look (if forward then i + 1 else i - 1)
	) else (
	  Listbox.see ~index:(`Num (playlist#length - 1)) lstBx;
	  Printf.printf "Not found  \n"; flush stdout;
	  searchPos := if forward then playlist#length else -1
	)
      in
      look !searchPos
    )
  in
  let search () =
    searchPattern := getString "Enter search pattern" !searchPattern;
    searchPos := -1;
    searchNext true
  in
  let activate pos =
    Listbox.see ~index:(`Num pos) lstBx;
    Listbox.activate ~index:(`Num pos) lstBx;
    Listbox.selection_clear ~first:(`Num 0) ~last:`End lstBx;
    Listbox.selection_set ~first:(`Num pos) ~last:(`Num pos) lstBx;
    Focus.set lstBx;
    Tk.update ()
  in
  let center () =
    activate !current
  in
  let histBack () =
    if !histPos > 0 then (
      decr histPos;
      activate playHist.(!histPos)
    )
  in
  let histFwd () =
    if !histPos < (!histCnt - 1) then (
      incr histPos;
      activate playHist.(!histPos)
    )
  in
  let keyPress e =
    let sym = e.Tk.ev_KeySymString
    and state = (int_of_string e.Tk.ev_State) mod 16
    in
    if sym = "Menu" || sym = "F2" then doPopup e
    else if sym = "period" then center ()
    else if sym = "less" then histBack ()
    else if sym = "greater" then histFwd ()
    else if state = 4 then begin
      if sym = "c" then copy ()
      else if sym = "m" then move ()
      else if sym = "x" then remove ()
      else if sym = "l" then lock ()
      else if sym = "n" then link ()
      else if sym = "r" then setRating ()
      else if sym = "p" then pqueue ()
      else if sym = "q" then queue ()
      else if sym = "f" then rename ()
      else if sym = "d" then mv ()
    end else if state = 8 then begin
      if sym = "f" then normName ()
    end else if sym = "slash" then search ()
    else if sym = "n" then searchNext true
    else if sym = "b" then searchNext false
    else if sym = "Escape" then quit ()
    else if _KEYDEBUG then (Printf.printf "sym=%s, state=%d\n" sym state);
    flush stdout
  in
  Tk.bind ~events:[`KeyPress] ~fields:[`State; `KeySymString; `Char]
  	~action:(fun e -> keyPress e) w;
  (*Tk.bind ~events:[`ButtonPressDetail 2; `KeyPressDetail "Menu";
    `KeyPressDetail "F2"; `KeyPressDetail "m"] ~fields:[`MouseX; `MouseY]
    ~action:(fun ev -> doPopup ev) w;*)
  Wm.protocol_set ~name:"WM_DELETE_WINDOW" ~command:quit w;

  center ();
  Tk.update ();
  listActive := true;
  processUpdateList ();
  Tkwait.window w

let updateList which entry =
  if !listActive then
    Queue.add (which, entry) updateListQueue
  else
    updateArray which entry

let parseLine line =
  let lst = String.nsplit line '|' in
  let len = List.length lst in
  { fn = List.nth lst 0;
    rating = if len > 1
                then int_of_string (List.nth lst 1)
                else !defaultScore;
    last = if len > 2 then float_of_string (List.nth lst 2) else 0.0;
    lists = if len > 3 then List.nth lst 3 else "A";
    cnt = if len > 4 then int_of_string (List.nth lst 4) else 0;
    flags = if len > 5 then List.nth lst 5 else "" }

let entryCompare a b = compare a.fn b.fn

let read fileName tvCount =
  let inFile = open_in fileName in
  let addNames lst =
    let slist = List.sort entryCompare lst in
    playlist#of_list slist;
  in
  let readFile () =
    let lst = ref [] in
    let cnt = ref 0 in
    try while true do
      let line = input_line inFile in
      let entry = parseLine line in
      if entry.lists <> "x" then (
        if entry.flags = "" then (
	  let flags =
	    (if String.contains entry.lists 'l' then "l" else "") ^
	    (if String.contains entry.lists 'n' then "n" else "")
	  in
	  entry.flags <- flags;
	);
	entry.lists <- delLetter entry.lists "l";
	entry.lists <- delLetter entry.lists "n";
	incr cnt;
        print_int !cnt; print_string "\r";
        Textvariable.set tvCount (string_of_int !cnt);
        lst := !lst @ [entry];
      );
      done;
    with _ -> addNames !lst
  in
  readFile ();
  close_in inFile;
  Gc.compact ()
