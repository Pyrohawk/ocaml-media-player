(* 2008-08-14 *)
(* 2007-10-27 ceh-Added ability to add directories to play list.
 * 2007-10-13 ceh-Began writing.
 *)

open Qlib
open Tk

(* LablTk Shim: Map missing Ttk widgets to standard Tk widgets *)
module Ttk_frame = Frame
module Ttk_button = Button
module Ttk_label = Label
module Ttk_sizegrip = Frame  (* Standard Tk lacks sizegrip; Frame acts as a harmless invisible placeholder *)
module Ttk_entry = Entry
module Ttk_scrollbar = Scrollbar
module Ttk_checkbutton = Checkbutton
module Ttk_radiobutton = Radiobutton
module Ttk_spinbox = Entry

(* These variables used to be in omp.ml but moved here to make them global *)
let _PROGNAME = "omp"
let _VERSION = "1.06.01"

let toolTipBG = `Color "#eaea00"

let ompDir =
  try
    (Sys.getenv "HOME") ^ "/.omp"
  with _ -> ".omp"
let omprc = ref (ompDir ^ "/omprc")
let imgDir = ompDir ^ "/images"
let ompSave = ompDir ^ "/omp.save"
let systemOmprc = ref "/etc/omprc"
let saveInterval = ref 10
let listsToPlay = ref "A"

let extensions = ref [
        ".ogg"; ".flac"; ".mp3"; ".mod"; ".wma"; ".m4a"; ".mp4"; ".flv";
        ".mov"; ".avi"; ".webm" ]


(* Command line options--Well, they were command line options in vux.
   Now they are either hard-coded or omprc options *)
let scoreList = ref (ompDir ^ "/scorelist")
let missingLog = ref (ompDir ^ "/missing")
let defaultScore = ref 50
let minimumAge = ref "4h"
let ageBonus = ref "0"
let minAge = ref 0.0
let agePenalty = ref 0.0
let checkRating = ref true
let checkRepeat = ref true
let updateRating = ref true
let forceNew = ref true
let selectMethod = ref "R"
let newSkipAge = ref 0
let autoLock = ref 0
let previousPath = ref ""

(* This is needed for the next option *)
let parseAge age default =
  try
  let measure = String.slice age (-1) 0 in
  let seconds =
    try
      float_of_string (String.slice age 0 (-1))
    with exn -> 0.0
  in
  if measure = "s" then seconds else
    let minutes = seconds *. 60. in
    if measure = "m" then minutes else
    let hours = minutes *. 60. in
    if measure = "h" then hours else
    let days = hours *. 24. in
    if measure = "d" then days else
    if measure = "w" then days *. 7. else
    if measure = "m" then days *. 30. else
    if measure = "y" then days *. 365. else
    minutes
  with _ -> (
    Printf.printf "Expected age, got '%s'\n%!" age;
    default)

let forgottenAge = ref "90d"
let forgottenAgeFloat = ref (parseAge !forgottenAge 7776000.)
let forgottenMin = ref 50
let showVideo = ref true

(* Experimental *)
let debug = ref false

(* Working variables *)
let current = ref 0
let maxSkip = ref 1
let played = ref 0
let stopPlayLoop = ref false
let exitPlayLoop = ref false
let playStat = ref 0
let searchStat = ref ""
let increase = ref 0
let decrease = ref 0
let oldRating = ref 0
let skipped = ref false
let stopAfter = ref false
let pauseAfter = ref false
let nowPlayingStart = ref 0
let paused = ref false
let restart = ref false
let previous = ref false
let stopQueue = ref false
let stopTime = ref "00:00"
let userfont = ref "fixed"
let fontSize = ref 9
let fontWeight = ref (`Bold : Tk.weight)
let nameWidth = ref 70

let ageMultiplier = Array.make 101 0.0

let threadCnt = ref 0

let _Idle = 0
let _Playing = 1
let _Searching = 2
let _Stopping = 3
let _Stop = 4
let _Done = 5
let statDesc = [| "Idle"; "Playing"; "Searching..."; "Stopping...";
	"Stop"; "Done" |]

let readline f =
  try input_line f
  with exn -> "EOF"

(* returns the value of s or min_int if s is not an integer *)
let ifInt s = try int_of_string s with exn -> min_int

let checkInt s msg default =
  let i = ifInt (String.trim s) in
  if i = min_int then begin
    Printf.printf "Integer expected in %s: found '%s'\n%!" msg s;
    default
  end else
    i   

let _ =
  (* Experimental *)
  if Array.length Sys.argv > 1 then (
    if Sys.argv.(1) = "debug" then debug := true
  );
  let getCurrent f =
    try
      let line = readline f in
      current := int_of_string line
    with exn -> current := (-1)
  in
  let getPath f =
    try
      let line = readline f in
      previousPath := line
    with exn -> ()
  in
  try
    let inFile = open_in ompSave in
    getCurrent inFile;
    getPath inFile;
    close_in inFile
  with exn -> current := (-1)

let processExtensions value =
  let newList = String.nsplit value ' ' in
  let checkDot s =
    let s' = String.trim s in
    if String.begins_with s' "." then s' else "." ^ s'
  in
  List.filter (fun a -> a <> "") (List.map checkDot newList)

let _ =
  try
    let inFile = open_in !omprc in
    let rec loop () =
      let line = readline inFile in
      if line = "EOF" then ()
      else if line = "" then loop ()
      else if String.sub line 0 1 = "#" then loop ()
      else if not (String.contains line '=') then (
        print_endline ("Bad omprc line: " ^ line);
	loop ()
      ) else (
        let (name, value) = String.split1 line '=' in
	ignore (match String.lowercase_ascii name with
	  "oggplayer" -> ()
	| "oggoptions" -> ()
	| "mp3player" -> ()
	| "mp3options" -> ()
	| "modplayer" -> ()
	| "modoptions" -> ()
        | "vidplayer" -> ()
        | "vidoptions" -> ()
        | "extensions" -> extensions := processExtensions value
	| "saveinterval" -> saveInterval := int_of_string value
	| "liststoplay" -> listsToPlay := value
	| "checkrating" -> checkRating := (value = "1")
	| "forcenew" -> forceNew := (value = "1")
	| "updaterating" -> updateRating := (value = "1")
	| "checkrepeat" -> checkRepeat := (value = "1")
	| "selectmethod" -> selectMethod := value
	| "minimumage" -> minimumAge := value
	| "agebonus" -> ageBonus := value
	| "newskipage" -> newSkipAge := checkInt value "NewSkipAge" !newSkipAge
	| "autolock" -> autoLock := checkInt value "AutoLock" !autoLock
	| "font" -> userfont := value
	| "fontsize" -> fontSize := checkInt value "FontSize" !fontSize
	| "fontweight" ->
		fontWeight := if value = "bold" then `Bold else `Normal
	| "namewidth" -> nameWidth := checkInt value "NameWidth" !nameWidth
	| "defaultscore" -> defaultScore := checkInt value
                             "DefaultScore" !defaultScore
	| "forgottenage" -> forgottenAge := value;
	                    forgottenAgeFloat := parseAge value !forgottenAgeFloat
	| "forgottenmin" -> forgottenMin :=
	                        checkInt value "ForgottenMin" !forgottenMin
	| "showvideo" -> showVideo := (value = "1")
	| _ -> print_endline ("Unrecognized option: " ^ line));
	loop ()
      )
    in
    loop ();
    close_in inFile
  with exn -> ()

let omprcSave () =
  try
    let outFile = open_out !omprc in
    let writeVal name value =
      output_string outFile name;
      output_string outFile "=";
      output_string outFile value;
      output_string outFile "\n";
    and writeLn s =
      output_string outFile s;
      output_string outFile "\n"
    in
    let bv v = if v then "1" else "0" in
    writeLn "# Use this file to set default omp options.";
    writeLn "# WARNING: omp does very little error checking.  Entries should";
    writeLn "# follow the pattern given exactly.  In particular, do not insert";
    writeLn "# spaces before the equal sign.";

    writeLn "\n# extensions that omp should handle (must be recognized by vlc)";
    writeVal "Extensions" (String.listCat !extensions " ");

    writeLn "\n# play list is saved after this many plays";
    writeVal "SaveInterval" (string_of_int !saveInterval);

    writeLn "\n# lists to play";
    writeVal "ListsToPlay" !listsToPlay;

    writeLn "\n# Check entry rating and roll random percentage or less to play";
    writeVal "Checkrating" (bv !checkRating);

    writeLn "\n# Play new songs without checking rating or age";
    writeVal "ForceNew" (bv !forceNew);

    writeLn "\n# Automatically adjust rating if entry isn't skipped";
    writeVal "UpdateRating" (bv !updateRating);

    writeLn "\n# Check entry for MinimumAge before playing";
    writeVal "CheckRepeat" (bv !checkRepeat);

    writeLn "\n# SelectMethod = R - Random, S - Sequential, O - Oldest first";
    writeVal "SelectMethod" !selectMethod;

    writeLn "\n# default score to assign new entries";
    writeVal "DefaultScore" (string_of_int !defaultScore);

    writeLn "\n# minimum age before an entry will replay, use the following suffixes:";
    writeLn "#  s = seconds , m = minutes (default), h = hours,";
    writeLn "#  d = days, w = weeks, m = months, y = years.";
    writeVal "MinimumAge" !minimumAge;

    writeLn "\n# Use AgeBonus logic.  See User's manual.";
    writeVal "AgeBonus" !ageBonus;

    writeLn "\n# Entry is considered 'new' for this number of plays (See ForceNew)";
    writeVal "NewSkipAge" (string_of_int !newSkipAge);

    writeLn "\n# Automatically lock rating after this number of plays/selections";
    writeLn "# 0 to disable";
    writeVal "AutoLock" (string_of_int !autoLock);

    writeLn "\n# Font to use for Playlist editor.  Best set within configuration";
    writeVal "Font" !userfont;

    writeLn "\n# Font size to use in Playlist editor.";
    writeVal "FontSize" (string_of_int !fontSize);

    writeLn "\n# Font weight to use in Playlist editor.  Either 'normal' or 'bold'";
    writeVal "FontWeight" (if !fontWeight = `Bold then "bold" else "normal");

    writeLn "\n# Number of spaces to use for file name in Playlist editor.  This is";
    writeLn "# approximate and affects the size of the entire window frame.";
    writeVal "NameWidth" (string_of_int !nameWidth);

    writeLn "\n# See user manual.";
    writeVal "ForgottenAge" !forgottenAge;
    writeVal "ForgottenMin" (string_of_int !forgottenMin);

    writeLn "\n# Show the video portion of files.  If 0, then only the sound is";
    writeLn "# played.  This defaults to 0 because vlc automatically sets the";
    writeLn "# input focus to the video window which can be annoying and certain";
    writeLn "# keys entered in the vlc window will confuse omp.";
    writeVal "ShowVideo" (bv !showVideo);

    close_out outFile
  with exn -> print_endline "Save to omprc failed"

let vlc = new Vlc.cVLC
let mixer = vlc#is_available

let calcAgeMult () =
  let step = tan (1.) /. 99. in
  for i = 1 to 100 do
    ageMultiplier.(i) <- 100. *. (atan (step *. (100. -. (float i))) ** 3.)
  done

let parseMinAge () =
  minAge := parseAge !minimumAge 7776000.;
  let bonus = parseAge !ageBonus 7776000. in
  if bonus > !minAge then (
    agePenalty := (bonus -. !minAge) /. 100.;
    let capture = ompDir ^ "/agecap.txt" in
    try
      if (Sys.command (ompDir ^ "/agebonus &> " ^ capture)) = 0 then (
        let inFile = open_in capture in
	for i = 1 to 100 do
	  let line = readline inFile in
	  ageMultiplier.(i) <- float_of_string line
	done;
	close_in inFile
      ) else calcAgeMult ()
    with exn -> calcAgeMult ()
  );
  ()

let elapsedTime t =
  let seconds = mod_float t 60. in
  let m = floor (t /. 60.) in
  let minutes = mod_float m 60. in
  let h = floor (m /. 60.) in
  let hours = mod_float h 24. in
  let days = floor (h /. 24.) in
  Printf.sprintf "%.0f days %02.0f:%02.0f:%02.0f" days hours minutes seconds

let random n = Random.int n

(* If this needs speeded up, it could probably be converted to any array
   so the list would not have to scanned all the way through.
   NOTE:  This will return the *last* match found. *)
let listSearch lst v =
  let i = ref 0 in
  let fnd = ref (-1) in
  let srch elem =
    if elem = v then fnd := !i;
    incr i
  in
  List.iter srch lst;
  !fnd

let addLetter s c =
  if String.contains s c.[0] then s else (s ^ c)

let delLetter s c =
  let pos = try String.index s c.[0] with exn -> -1 in
  if pos < 0 then
    s
  else
    (String.sub s 0 pos) ^ (String.sub s (pos + 1) (String.length s - pos - 1))

let toggleLetter s c =
  let pos = try String.index s c.[0] with exn -> -1 in
  if pos < 0 then
    s ^ c
  else
    (String.sub s 0 pos) ^ (String.sub s (pos + 1) (String.length s - pos - 1))

let lchop s = if s = "" then "" else String.sub s 1 (String.length s - 1)
let rchop s = if s = "" then "" else String.sub s 0 (String.length s - 1)

let to_time seconds =
  let hr = seconds / 3600
  and min = (seconds mod 3600) / 60
  and sec = (seconds mod 60)
  in
  if hr > 0 then
    Printf.sprintf "%d:%02d:%02d" hr min sec
  else
    Printf.sprintf "%02d:%02d" min sec
