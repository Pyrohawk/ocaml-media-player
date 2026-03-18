(* 2007-09-21 *)
(* 2007-10-27 ceh-Added ability to add directories to play list.
 * 2007-10-13 ceh-I decided to just write it from scratch.  I think I can do
 *	that just about as quickly as trying to convert it and the results
 *	should be cleaner anyway.  Besides there are a number of features
 *	I want to add and as many or more in vux I don't care to support.
 *)

open Qlib
open G
open Playlist
open Confedit
open Tk

let lock = Mutex.create ()

(* 2008-03-21 Tooltips *)
let tipBtn = ref ""
let tipTimer = ref None
let tipMsg = ref None

let saveList () =
  let tmp = !scoreList ^ ".tmp" in
  let outFile = open_out tmp in
  let write entry =
    let outLine = Printf.sprintf "%s|%d|%0.0f|%s|%d|%s\n"
      entry.fn entry.rating entry.last entry.lists entry.cnt entry.flags
    in
    output_string outFile outLine
  in
  playlist#iter write;
  close_out outFile;
  Sys.remove !scoreList;
  Sys.rename tmp !scoreList;
  let outFile = open_out ompSave in
  let outLine = Printf.sprintf "%d\n" (!current - 1) in
  output_string outFile outLine;
  output_string outFile (!previousPath ^ "\n");
  close_out outFile;
  print_endline "Playlist saved."

let main () =
  heapCheck "main";
  (* Added quitting to get rid of code after call to confEdit trying to
     update destroyed windows *)
  let running = ref true in
  if not (Sys.file_exists !scoreList) then (
    msg (!scoreList ^ " does not exist.  Please create it with an initial playlist.") "E";
    exit 1
  );
  Random.self_init ();
  parseMinAge ();

  (* oldest sequential play *)
  let dateSort = ref [] in
  let oldest = ref 0 in
  
  (* avoid multiple calls to get_length *)
  let entry_length = ref 0 in

  (* v0.20.00 favorite oldies search - "forgotten".  See forgottenSearch *)
  let forgottenRating = ref 100
  and forgottenNext = ref 1
  and forgottenFound = ref false
  in
  (* 2014-04-27 modified from (101 - !forgottenRating) to make checks
     closer together *)
  let forgottenDelay () =
    if !forgottenFound then
      max 1 ((102 - !forgottenRating) / 2)
    else
      1
  in
  let forgottenCnt = ref (forgottenDelay ()) in

  (* Wm.title_set window "omp - OCaml Multimedia Player"; *)
  (* Style.map `TButton "-background" [(`Alternate, "#9090ff")]; *)
  let playImage = Imagephoto.create ~file:(imgDir ^ "/play.gif") () in
  let pauseImage = Imagephoto.create ~file:(imgDir ^ "/pause.gif") () in
  let stopImage = Imagephoto.create ~file:(imgDir ^ "/stop.gif") () in
  let exitImage = Imagephoto.create ~file:(imgDir ^ "/exit.gif") () in
  let nextImage = Imagephoto.create ~file:(imgDir ^ "/next.gif") () in
  let previousImage = Imagephoto.create ~file:(imgDir ^ "/previous.gif") () in
  let randomImage = Imagephoto.create ~file:(imgDir ^ "/random.gif") () in
  let sequenceImage = Imagephoto.create ~file:(imgDir ^ "/sequence.gif") () in
  let skipdownImage = Imagephoto.create ~file:(imgDir ^ "/skipdown.gif") () in
  let afterImage = Imagephoto.create ~file:(imgDir ^ "/after.gif") () in
  let downImage = Imagephoto.create ~file:(imgDir ^ "/down.gif") () in
  let upImage = Imagephoto.create ~file:(imgDir ^ "/up.gif") () in
  let lockImage = Imagephoto.create ~file:(imgDir ^ "/lock.gif") () in
  let saveImage = Imagephoto.create ~file:(imgDir ^ "/save.gif") () in
  let percentImage = Imagephoto.create ~file:(imgDir ^ "/percent.gif") () in
  let atsignImage = Imagephoto.create ~file:(imgDir ^ "/atsign.gif") () in
  let menuImage = Imagephoto.create ~file:(imgDir ^ "/menu.gif") () in

  let frame = Frame.create ~width:600 ~height:600 window in
  let labels = Frame.create frame in
  let lblVer = Label.create ~text:("V." ^ _VERSION) labels in
  let lbl = Label.create ~text:"Playlist: " labels in
  Tk.pack ~side:`Left [lblVer; lbl];
  let lblStat = Label.create ~text:"Status" labels in
  let lblSearch = Label.create ~text:"*" labels in
  let tvCount = Textvariable.create () in
  let txCount = Entry.create ~width:6 ~textvariable:tvCount ~takefocus:false labels in
  let lblIncDec = Label.create ~text:"" labels in
  let lblVol = Label.create ~text:"" labels in
  let lblPos = Label.create ~text:"" labels in
  let lblStop = Label.create ~text:"" labels in
  Tk.pack ~side:`Left [txCount];
  Tk.pack ~side:`Left [lblStat; lblSearch; lblIncDec; lblVol; lblPos; lblStop];
  Tk.pack ~side:`Top  [labels];
  let tvNowPlaying = Textvariable.create () in
  let txtNowPlaying =
    Entry.create ~textvariable:tvNowPlaying ~width:80 ~takefocus:false frame
  in
  Tk.pack [txtNowPlaying];
  let buttons = Frame.create frame in
  let btnPause = Button.create ~image:pauseImage buttons in
  let btnAfter = Button.create ~image:afterImage buttons in
  let btnLock = Button.create ~image:lockImage buttons in
  let btnSequence = Button.create ~image:sequenceImage buttons in
  let btnRandom = Button.create ~image:randomImage buttons in
  let btnRating = Button.create ~image:percentImage buttons in
  let btnRepeat = Button.create ~image:atsignImage buttons in
  let btnMenu = Button.create ~image:menuImage buttons in
  
  let updateIncDec rate =
    let text = Printf.sprintf "%d(^%d,v%d)" rate !increase !decrease in
    Label.configure lblIncDec ~text;
  in
  let updateMethod s =
    selectMethod := s;
  let (rr, rs) = match !selectMethod with
        "O" -> (`Sunken, `Sunken)
      | "R" -> (`Sunken, `Raised)
      | _   -> (`Raised, `Sunken)     (* "S", sequential *)
    in
    Button.configure ~relief:rr btnRandom;
    Button.configure ~relief:rs btnSequence
  in
  let changeMethod s =
    dateSort := [];
    if s = "R" || s = "S" then
      updateMethod s
    else if s = "r" then
      if !selectMethod = "R" then
        updateMethod "S"
      else
        updateMethod "R"
    else (
      let temp = ref [] in
      for i = 0 to playlist#length - 1 do
        let entry = playlist#get i in
	let this = Printf.sprintf "%10.0f%06d" entry.last i in
(*	if i = 0 || i = playlist#length - 1 then print_endline this; *)
	temp := !temp @ [this];
      done;
(*      flush stdout; *)
      temp := List.sort compare !temp;
      for i = 0 to playlist#length - 1 do
        let v = int_of_string (String.sub (List.nth !temp i) 10 6) in
        dateSort := !dateSort @ [v];
      done;
      oldest := 0;
(*      for i = 0 to playlist#length - 1 do
        Printf.printf "%s %d\n" (List.nth !temp i) (List.nth !dateSort i)
      done; *)
      updateMethod "O"
    )
  in
  let doUpdateRating check update =
    checkRating := check;
    updateRating := update;
    let r = if !checkRating && !updateRating then `Sunken else `Raised in
    Button.configure ~relief:r btnRating
  in
  let setPressed b =
    if b then `Sunken else `Raised
  in
  let updateRepeat b =
    checkRepeat := b;
    Button.configure ~relief:(setPressed !checkRepeat) btnRepeat
  in
  let updateLockBtn locked =
    Button.configure ~relief:(setPressed locked) btnLock;
  in
  let updateVol () =
    Mutex.lock lock;
    let (vm, mm) = vlc#getMaster in
    Mutex.unlock lock;
    let text = Printf.sprintf "Vol. %d/%d" vm mm in
    Label.configure lblVol ~text;
  in
  let updatePos length pos =
    let pct = int_of_float (float pos *. 100. /. float length)
    and slen = to_time length
    and spos = to_time pos
    in
    let text = Printf.sprintf "|%s/%s(%d%%)|" spos slen pct in
    Label.configure lblPos ~text
  in
  let updateStop () =
    let msg =
      (if !stopQueue then "Q" else "") ^
      (if !stopTime = "00:00" then "" else !stopTime)
    in
    let text = if msg <> "" then "Stop @" ^ msg else "" in
    Label.configure lblStop ~text
  in

  (* Set up inter-thread communications *)
  (*let channel = Event.new_channel () in*)

  let playLoop () =
    incr threadCnt;
    if !threadCnt > 1 then Printf.printf "Thread #%d\n" !threadCnt;
    let setPlay num =
      Mutex.lock lock;
      playStat := _Playing;
      current := num;
      Mutex.unlock lock
    in
    let initSearch () =
      Mutex.lock lock;
      playStat := _Searching;
      searchStat := "";
      Mutex.unlock lock;
    in
    let addSearch srch =
      Mutex.lock lock;
      searchStat := !searchStat ^ srch;
      Mutex.unlock lock
    in
    let setStop () =
      Mutex.lock lock;
      playStat := _Stopping;
      Mutex.unlock lock
    in
    let is_playing () =
      let b = ref true in
      Mutex.lock lock;
      b := vlc#is_playing;
      Mutex.unlock lock;
      !b
    in
    let playedCnt = ref 0 in
    let play1 num =
      entry_length := 0;
      setPlay num;
      if not !restart && not !previous then pushHist num;
      restart := false;
      previous := false;
      (*let stat = Printf.sprintf "Playing %d" num in
      ignore (Event.send channel stat); *)
      let entry = playlist#get num in
      nowPlayingStart := 0;
      
      let unsupported fileName =
        not (List.exists (fun a -> a = (getExtension fileName)) !extensions)
      in
      if unsupported entry.fn then
        print_endline ("Unrecognized file type: " ^ entry.fn)
      else (
        incr playedCnt;
       	skipped := false;

	let ratingLocked = String.contains entry.flags 'l' in
	oldRating := entry.rating;
        increase :=
	  if ratingLocked then entry.rating
	  else min 100 (entry.rating + min 5 ((100 - entry.rating) / 10));
	decrease :=
	  if ratingLocked then entry.rating
	  else max 0 (entry.rating - min 5 (entry.rating / 10));
	Mutex.lock lock;
        vlc#play entry.fn;
        Mutex.unlock lock;
	let rec wait () =
	  Unix.sleep 1;
	  if !exitPlayLoop || !stopPlayLoop || !skipped || !restart ||
	  	!previous then (
	    Mutex.lock lock;
	    vlc#stop;
	    Mutex.unlock lock;
            (*Thread.join handle;*)
	  ) else if not (is_playing ()) then
	    playStat := _Done
	  else if !playStat <> _Done then wait ()
	in
	wait ();
	if not !stopPlayLoop && not !restart && not !previous then (
	  (* entry can be changed while song is playing in other places
	     so reload it *)
          let entry = playlist#get num in
	  let newRating =
	    if not !updateRating || entry.rating <> !oldRating then
	      entry.rating
	    else if !skipped then
	      !decrease
	    else
	      !increase
	  in
	  entry.cnt <- entry.cnt + 1;
	  entry.rating <- newRating;
	  entry.last <- Unix.time ();
	  if not ratingLocked && !autoLock > 0 && entry.cnt >= !autoLock then
	    entry.flags <- entry.flags ^ "l";
	  if newRating <> !oldRating then
	    Printf.printf "New rating = %d\n" newRating;
	  updateList num entry
	)
      )
    in
    let playFlag = ref false in
    let attempts = ref 0 in
    let rec findplay skipcnt =
      while !pauseAfter do Unix.sleep 1 done;
      let notInList lsts =
        let len = String.length lsts in
        let rec check i =
	  if i >= len then true else
	    let ch = lsts.[i] in
	    let isInList = String.contains !listsToPlay ch in
	    if isInList then false
	    else check (i + 1)
	in
	check 0
      in
      if skipcnt = 0 && !playFlag then (
        if !saveInterval > 0 && !playedCnt mod !saveInterval = 0 then
	  saveList ();
	initSearch ();
      );
      attempts := if !playFlag then 0 else !attempts + 1;
      playFlag := false;
      if !previous then (
        let prevNum = popHist () in
	let prevNum = if prevNum = !current then popHist () else prevNum in
	if prevNum >= 0 then pushQueue prevNum;
      );
      let queued = List.length !playQueue in

      (* In random selection mode, try to make sure favorite songs come up at
	least every 3 months.  Favorite is user defined !forgottenMin (default
	50%) or higher.
	Before doing the normal random selection, every so many times through,
	(function forgottenDelay determines how many songs to play
	before the forgotten check) traverse the list
	looking for every song with a rating of !forgottenRating beginning the
	search at !forgottenNext.  If age > !forgottenAge, then select entry.
	Otherwise, set !forgottenNext to entryNum + 1 and defer to the old
	random selector.  When !forgottenNext reaches eof, decrement
	!forgottenRating and reset !forgottenNext.  When !forgottenRating
	< !forgottenMin, decrement !forgottenAge and reset !forgottenRating.
      *)
      let forgottenSearch () =
	let time = Unix.time () in
	let rec loop () =
	  if !forgottenNext >= playlist#length then begin
	    decr forgottenRating;
	    forgottenNext := 0;
	    if !forgottenRating < !forgottenMin then begin
	      forgottenRating := 100;
	      forgottenAgeFloat := !forgottenAgeFloat -. (24. *. 60. *. 60.)
	    end;
	    (-1)
	  end else begin
	    let i = !forgottenNext in
	    incr forgottenNext;
	    let entry = playlist#get i in
	    if entry.rating = !forgottenRating &&
		time -. entry.last > !forgottenAgeFloat &&
		not (notInList entry.lists) then begin
              forgottenFound := true;
	      i
	    end else
	      loop ()
	  end
	in
	if !forgottenCnt <= 0 then begin
	  forgottenCnt := forgottenDelay ();
	  loop ()
	end else
	  (-1)
      in

      let rangeSearch () =
	let range = (playlist#length + 1000) / 1000 in
	let r = Random.int (playlist#length - 1) in
	let s1 = max (r - range) 0
	and s2 = min (r + range) (playlist#length - 1)
	in
	let rec rloop i oldest which =
	  if i > s2 then which else
	    let entry = playlist#get i in
	    if entry.last < oldest then
	      rloop (i + 1) entry.last i
	    else
	      rloop (i + 1) oldest which
	in
	rloop s1 max_float s1
      in

      let which = match !playQueue with
          hd :: tl -> playQueue := tl; dirtyQueue := true; hd
        | _ -> match !selectMethod with
		  "R" ->
			let i = forgottenSearch () in
			Printf.printf 
			  "forgotten: rating=%d, next=%d, cnt=%d\n%!"
			    !forgottenRating !forgottenNext !forgottenCnt;
			if i >= 0 then i else rangeSearch ()
		| "O" -> incr oldest;
			 if !oldest >= playlist#length then oldest := 0;
			 List.nth !dateSort !oldest
		| _ -> incr current;
		        if !current >= playlist#length then current := 0;
			!current
      in
      let entry = playlist#get which in
      let printSelection result =
        Printf.printf "%d. %s - %s\n" which entry.fn result;
	flush stdout
      in
      let time = Unix.time () in
      let timeToStop () =
        if !stopTime = "00:00" then
	  false
	else (
	  let now = Unix.localtime time in
	  let sNow =
	    Printf.sprintf "%02d:%02d" now.Unix.tm_hour now.Unix.tm_min
	  in
	  sNow >= !stopTime
	)
      in
      let doStop msg =
        print_endline msg;
	saveList ();
	playStat := _Stop
      in

      (* Avoid infinite loop caused by bad playlist or some other error *)
      if !attempts > playlist#length then doStop "Nothing to play";

      let modifiedMinAge =
        if !agePenalty > 0.0 then
	  !minAge +. (!agePenalty *. ageMultiplier.(entry.rating))
	else
	  !minAge
      in

      if !debug then (
        let limit x lim = if x > lim then (x / lim, x mod lim) else (0, x) in
        let (dmi, dse) = limit (truncate modifiedMinAge) 60 in
	let (dhr, dmi) = limit dmi 60 in
	let (dda, dhr) = limit dhr 24 in
        Printf.printf "Rating=%d, Penalty=%.2f, multiplier=%.2f, minAge=%.0f, \
	  modAge=%dd%02d:%02d:%02d\n"
	  entry.rating !agePenalty ageMultiplier.(entry.rating) !minAge
	  dda dhr dmi dse;
	flush stdout;
      );

      (* Roll "dice" now so it can be reported. *)
      let roll = Random.int 100 in
      if !exitPlayLoop then (
        print_endline "Stop";
        setStop ();
      ) else if !stopAfter then (
        doStop "Stop after";
      ) else if timeToStop () then (
        doStop ("Stop after " ^ !stopTime);
      ) else if not (Sys.file_exists entry.fn) then (
        printSelection "Missing";
	findplay skipcnt
      ) else if queued > 0 then (
        printSelection "Queue";
	addSearch (Printf.sprintf "Q(%d)" queued);
	playFlag := true
      ) else if !stopQueue then (
        doStop "Stop after queue";
	stopQueue := false;
      ) else if notInList entry.lists then (
        (* printSelection "Not in list"; *)
        findplay skipcnt
      ) else if entry.rating = 0 then (
        (*printSelection "Rating = 0";*)
	findplay skipcnt
      ) else if skipcnt > !maxSkip || entry.last = 0.0 then (
        printSelection "Force play";
        addSearch "!";
	playFlag := true
      ) else if !checkRepeat && (entry.cnt > !newSkipAge || entry.rating < 50)
          && time -. entry.last < modifiedMinAge then (
        let s = elapsedTime (time -. entry.last) in
        printSelection (Printf.sprintf "Too recent: %s" s);
        addSearch "@";
        findplay (skipcnt + 1)
      ) else if !checkRating && roll >= entry.rating then (
        printSelection
	  (Printf.sprintf "Failed %d%% chance with %d" entry.rating roll);
	entry.last <- Unix.time ();
	updateList which entry;
        addSearch "%";
        findplay (skipcnt + 1)
      ) else (
        printSelection "Play";
        addSearch "+";
	playFlag := true
      );
      if !playFlag then (
	decr forgottenCnt;
        play1 which;
	while !restart do play1 which done;
	if String.contains entry.flags 'n' then pushQueue (which + 1);
	while !stopPlayLoop do Unix.sleep 1 done;
	findplay 0
      )
    in
    findplay 0
  in
  let rec scrollName () =
    let entry = playlist#get !current in
    let fmt = String.listCat (formatEntry entry !current true) " "
    in
    let len = String.length fmt in
    if len > 80 then (
      Textvariable.set tvNowPlaying
        ((String.slice fmt !nowPlayingStart 0) ^
        "    " ^ fmt);
      incr nowPlayingStart;
      if !nowPlayingStart >= len then
        nowPlayingStart := 0;
    ) else if !nowPlayingStart = 0 then (
      Textvariable.set tvNowPlaying fmt;
      nowPlayingStart := 1;
    );
    Timer.set ~ms:333 ~callback:scrollName
  in
  let rec poll () =
    Thread.delay 0.1;
    Mutex.lock lock;
    Label.configure lblSearch ~text:!searchStat;
    if !playStat = _Playing then (
      let text = Printf.sprintf "%s %d" statDesc.(!playStat) !current in
      Label.configure lblStat ~text;
      let entry = playlist#get !current in
      updateIncDec entry.rating;
      let ratingLocked = String.contains entry.flags 'l' in
      updateLockBtn ratingLocked;
      if !entry_length = 0 then entry_length := vlc#get_length;
      let pos = vlc#get_time in
      updatePos !entry_length pos;
    ) else if !playStat = _Stop then (
      Tk.closeTk ();
    ) else (
      Label.configure lblStat ~text:statDesc.(!playStat);
    );
    Mutex.unlock lock;
    Timer.set ~ms:500 ~callback:poll
  in
  let pause () =
    if !pauseAfter then (
      pauseAfter := false;
      Button.configure ~relief:`Raised btnPause
    ) else if !playStat = _Playing then (
      Mutex.lock lock;
      vlc#pause;
      Mutex.unlock lock;
      paused := not !paused;
      Button.configure ~relief:(setPressed !paused) btnPause;
    );
  in
  let doPauseAfter () =
    if !pauseAfter then pause ()
    else (
      pauseAfter := true;
      Button.configure ~relief:`Sunken btnPause
    )
  in
  let play () =
    if !pauseAfter || !paused then (
      pause ()
    ) else if !stopPlayLoop then (
      stopPlayLoop := false
    ) else if !playStat = _Playing then (
      restart := true;
    ) else (
      ignore (Thread.create playLoop ());
      Timer.set ~ms:100 ~callback:poll;
      Thread.delay 0.5;
      scrollName ()
    )
  in
  let changeRating () =
    let entry = playlist#get !current in
    entry.rating <- !increase;
    updateList !current entry
  in
  let upBig () =
    increase := min 100 (!increase + 5);
    decrease := !increase;
    changeRating ()
  in
  let up () =
    if !increase = !decrease then increase := min 100 (!increase + 1);
    decrease := !increase;
    changeRating ()
  in
  let downBig () =
    decrease := max 0 (!decrease - 5);
    increase := !decrease;
    changeRating ()
  in
  let down () =
    if !increase = !decrease then decrease := max 0 (!decrease - 1);
    increase := !decrease;
    changeRating ()
  in
  let lock_rating () =
    let entry = playlist#get !current in
    (* Note that locked = old status *)
    let locked =  String.contains entry.flags 'l' in
    let strip c = if c = 'l' then "" else String.make 1 c in
    let newFlags = if locked
      then String.filter strip entry.lists
      else entry.flags ^ "l"
    in
    entry.flags <- newFlags;
    updateList !current entry;
    increase :=
	if locked then
	  min 100 (entry.rating + min 5 ((100 - entry.rating) / 10))
	else 
	  entry.rating;
    decrease :=
	if locked then
	  max 0 (entry.rating - min 5 (entry.rating / 10))
	else
	  entry.rating;
    updateLockBtn (not locked)
  in
  let skip () =
    if !playStat = _Playing then (
      let entry = playlist#get !current in
      decrease := max 0 (entry.rating - min 5 (entry.rating / 10));
      skipped := true
    )
  in
  let next () =
    if !playStat = _Playing then (
      if !increase <> !decrease then (
        let entry = playlist#get !current in
        increase := entry.rating;
        decrease := entry.rating;
      );
      skipped := true
    )
  in
  let afterStatus () =
    let r =
      if !stopAfter || !stopQueue || !stopTime <> "00:00" then `Sunken
      else `Raised
    in
    Button.configure ~relief:r btnAfter;
    updateStop ()
  in
  let after () =
    if !stopQueue then stopQueue := false
    else if !stopTime <> "00:00" then stopTime := "00:00"
    else stopAfter := not !stopAfter;
    afterStatus ()
  in
  let clickRandom () =
    updateMethod "R"
  in
  let clickSequence () =
    updateMethod "S"
  in
  let clickRating () =
    if !checkRating then (
      if !updateRating then
        doUpdateRating false false
      else
        doUpdateRating true true
    ) else
      doUpdateRating true false
  in
  let clickRepeat () =
    updateRepeat (not !checkRepeat)
  in
  let selList () =
    let resp = getString "Enter list(s) to play:" "A" in
    listsToPlay := if resp = "" then "A" else resp;
    let text = "List: " ^ !listsToPlay ^ ", Total: " in
    Label.configure lbl ~text;
  in
  let stopAt () =
    let resp = getString "Enter time to stop (HH:MM):" !stopTime in
    stopTime := if resp = "" then "00:00" else resp;
    afterStatus ()
  in
  let editPlaylist () =
    Playlist.edit ();
    Textvariable.set tvCount (string_of_int playlist#length)
  in
  let prev () =
    if !playStat = _Playing then previous := true
  in
  let stopAfterQueue () =
    stopQueue := true;
    afterStatus ()
  in
  let stop () =
    if !playStat = _Playing then (
      stopPlayLoop := true;
      Unix.sleep 2
    )
  in
  let save () =
    saveList ();
  in
  let quit () =
    exitPlayLoop := true;
    (* 2008-08-07 Added to not update rating if program is exited. *)
    stopPlayLoop := true;
    Unix.sleep 2;
    saveList ();
    flush stdout;
    running := false;
    Tk.closeTk ();
  in

  let configure () =
    let saveMethod = !selectMethod in
    confEdit ();
    if !running then (
      if !selectMethod <> saveMethod then
        changeMethod !selectMethod;
      doUpdateRating !checkRating !updateRating;
      updateRepeat !checkRepeat
    )
  in

  let doMenu () =
    let popup = Menu.create ~typ:`Normal frame in
    Menu.add_command ~label:"Select play list (S)" ~command:selList popup;
    Menu.add_command ~label:"Playlist editor (p)" ~command:editPlaylist popup;
    Menu.add_command ~label:"Stop after queue (q)" ~command:stopAfterQueue popup;
    Menu.add_command ~label:"Stop after time (t)" ~command:stopAt popup;
    Menu.add_command ~label:"Preferences (P)" ~command:configure popup;
    let x = Winfo.rootx btnMenu + 13 in
    let y = Winfo.rooty btnMenu + 13 in
    Menu.popup ~x ~y popup
  in

  let doPress btn =
    let state r =
      Button.configure ~relief:r btn;
      Tk.update ()
    in
    let unPress () = state `Raised in
    state `Sunken;
    Timer.set ~ms:1000 ~callback:unPress;
  in

  let btnPrev = Button.create ~image:previousImage ~command:prev buttons in
  let btnPlay = Button.create ~image:playImage ~command:play buttons in
  Button.configure btnPause ~command:pause;
  let btnStop = Button.create ~image:stopImage ~command:stop buttons in
  let btnUp = Button.create ~image:upImage ~command:up buttons in
  let btnDown = Button.create ~image:downImage ~command:down buttons in
  let btnSkip = Button.create ~image:skipdownImage ~command:skip buttons in
  let btnNext = Button.create ~image:nextImage ~command:next buttons in
  Button.configure btnAfter ~command:after;
  Button.configure btnLock ~command:lock_rating;
  Button.configure btnRandom ~command:clickRandom;
  Button.configure btnSequence ~command:clickSequence;
  Button.configure btnRating ~command:clickRating;
  Button.configure btnRepeat ~command:clickRepeat;
  let btnSave = Button.create ~image:saveImage ~command:save buttons in
  Button.configure btnMenu ~command:doMenu;
  let btnExit = Button.create ~image:exitImage ~command:quit buttons in
  Tk.pack ~side:`Left [btnPrev; btnPlay; btnPause; btnStop; btnNext; btnSkip;
    btnUp; btnDown; btnAfter; btnLock; btnRandom; btnSequence;
    btnRating; btnRepeat; btnSave; btnMenu; btnExit];
  Tk.pack ~side:`Bottom [buttons];
  Tk.pack ~expand:true ~fill:`Both [frame];
  let doRead () =
    heapCheck "doRead start";
    Playlist.read !scoreList tvCount;
    maxSkip := truncate (sqrt (float playlist#length));
    heapCheck "doRead end";
    if mixer then (
      updateVol ()
    );
    changeMethod !selectMethod;
    doUpdateRating !checkRating !updateRating;
    updateRepeat !checkRepeat;
    flush stdout;
  in
  Timer.set ~ms:100 ~callback:doRead;

  let removeTip () =
    (match !tipTimer with
      Some a -> Timer.remove a
     | None -> ()
    );
    (match !tipMsg with
      Some a -> Tk.destroy a
     | None -> ()
    );
  in

  let doTip () =
    let toolMsg () =
      let (msg, which) = match !tipBtn with
	 "prev" -> ("Play previous entry (z)", btnPrev)
	| "play" -> ("Begin playing or restart current entry (x)", btnPlay)
	| "pause" -> ("Pause/Resume (c)", btnPause)
	| "stop" -> ("Stop playing (v)", btnStop)
	| "up" -> ("Increase entry rating (Up)", btnUp)
	| "down" -> ("Decrease entry rating (Down)", btnDown)
	| "skip" -> ("Decrease entry rating and play next entry (n)", btnSkip)
	| "next" -> ("Play next entry without changing rating (b)", btnNext)
	| "after" -> ("Exit omp after this entry (a)", btnAfter)
	| "lock" -> ("Lock this entry's current rating (l)", btnLock)
	| "rand" -> ("Play entries in random order (r)", btnRandom)
	| "seq" -> ("Play entries in sequential order (r)", btnSequence)
	| "%" -> ("Use and adjust entry ratings (%)", btnRating)
	| "@" -> ("Check minimum frequency before playing entry (@)", btnRepeat)
	| "save" -> ("Save playlist (s)", btnSave)
	| "menu" -> ("Display omp menu (m)", btnMenu)
	| "exit" -> ("Exit omp (^w)", btnExit)
	| _ -> ("", btnMenu)
      in
      let x = Winfo.rootx which in
      let y = Winfo.rooty which in
      let screenheight = Winfo.screenheight window in
      let msgWindow = Toplevel.create ~background:(`Color "") ~borderwidth:1
      	~takefocus:false window
      in
      Wm.overrideredirect_set msgWindow true;
      let newy = if y + 50 > screenheight then y - 20 else y + 30 in
      let wset = Printf.sprintf "+%d+%d" x newy in
      Wm.geometry_set msgWindow wset;
      let msgFrame = Frame.create msgWindow in
      let msgWidget = Label.create ~background:toolTipBG ~foreground:`Black
	~text:msg msgFrame
      in
      Tk.pack [msgWidget];
      Tk.pack [msgFrame];
      msgWindow
    in
    tipMsg := Some (toolMsg ());
    tipTimer := Some (Timer.add ~ms:6000 ~callback:removeTip)
  in

  let mouseEL dir btn =
    (* whether coming or going, we need to kill any messages and timers *)
    removeTip ();
    if dir = 0 then (
      (* mouse entered new button, set timer for tip *)
      tipTimer := Some (Timer.add ~ms:300 ~callback:doTip);
      tipBtn := btn
    )
  in

  let jump amt =
    Mutex.lock lock;
    vlc#jump amt;
    Mutex.unlock lock
  in
  
  let doChangeVol state dir =    
    Mutex.lock lock;
    let amt = (if dir = "+" then 1 else -1) * (if state = 4 then 10 else 1) in
    vlc#adjust_volume amt;
    Mutex.unlock lock;
    updateVol ()
(* print_endline "from updateVol"; flush stdout *)
  in

  let keyPress e =
(* print_endline "in keyPress"; flush stdout; *)
    let ch = e.Tk.ev_Char
    and sym = e.Tk.ev_KeySymString
    and state = (int_of_string e.Tk.ev_State) mod 16
    in
    if sym = "Up" then (doPress btnUp; up ())
    else if sym = "Down" then (doPress btnDown; down ())
    else if sym = "Prior" then upBig ()
    else if sym = "Next" then downBig ()
    else if sym = "Right" then jump 10
    else if sym = "Left" then jump (-10)
    else if state = 4 && sym = "Right" then jump 60
    else if state = 4 && sym = "Left" then jump (-60)
    else if state = 4 && (sym = "Escape" || sym = "w") then (
      doPress btnExit;
      quit ()
    ) else if ch = "z" then (doPress btnPrev; prev ())
    else if ch = "x" then (doPress btnPlay; play ())
    else if ch = "c" || ch = " " then pause ()
    else if ch = "C" then doPauseAfter ()
    else if ch = "v" then (doPress btnStop; stop ())
    else if ch = "b" then (doPress btnNext; next ())
    else if ch = "n" then (doPress btnSkip; skip ())
    else if ch = "s" then (doPress btnSave; save ())
    else if ch = "+" || ch = "-" then doChangeVol state ch
    else if ch = "q" then stopAfterQueue ()
    (* Added 0.09.04 *)
    else if ch = "a" then after ()
    else if ch = "l" then lock_rating ()
    else if ch = "r" then changeMethod "r"
    else if ch = "%" then clickRating ()
    else if ch = "@" then clickRepeat ()
    else if ch = "m" then doMenu ()
    else if ch = "p" then editPlaylist ()
    else if ch = "S" then selList ()
    else if ch = "t" then stopAt ()
    (* Added 0.10.00 *)
    else if ch = "o" then changeMethod "o"
    else if ch = "P" then configure ()
    (* *)
    else if ch = "Q" then (
      (* temporary: dump queue *)
      print_endline "Queue";
      for i = 0 to (List.length !playQueue) - 1 do
        Printf.printf "%d. %d\n" i (List.nth !playQueue i)
      done;
      flush_all ()
    ) else if ch = "h" then (
      print_endline "History";
      for i = 0 to !histCnt - 1 do
        Printf.printf "%d. %d\n" i playHist.(i)
      done;
      flush_all ()
    ) else if ch = "w" then (
      Printf.printf "current=%d\n" !current;
      flush_all ()
    ) else ()
  in

  Tk.bind ~events:[`KeyPress] ~fields:[`State; `KeySymString; `Char]
  	~action:(fun e -> keyPress e) window;
  let toolBind btn desc =
    Tk.bind ~events:[`Enter] ~action:(fun e -> mouseEL 0 desc) btn;
    Tk.bind ~events:[`Leave] ~action:(fun e -> mouseEL 1 desc) btn;
  in
  toolBind btnPrev "prev";
  toolBind btnPlay "play";
  toolBind btnPause "pause";
  toolBind btnStop "stop";
  toolBind btnUp "up";
  toolBind btnDown "down";
  toolBind btnSkip "skip";
  toolBind btnNext "next";
  toolBind btnAfter "after";
  toolBind btnLock "lock";
  toolBind btnRandom "rand";
  toolBind btnSequence "seq";
  toolBind btnRating "%";
  toolBind btnRepeat "@";
  toolBind btnSave "save";
  toolBind btnMenu "menu";
  toolBind btnExit "exit";

  Tk.mainLoop ()
  
let _ = main ()
