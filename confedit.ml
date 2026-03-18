(* 2008-08-14 *)

open Qlib
open Tk
open G
open Playlist

(* let dbg n =
  Printf.printf "Debug %d\n" n;
  flush stdout
*)

let selectFont () =
  let families = List.sort compare (Font.families ()) in
  let tvSize = Textvariable.create () in
  let tvWidth = Textvariable.create () in
  let tvWeight = Textvariable.create () in
  let tvSample = Textvariable.create () in
  let family = ref !userfont in
  let w = Toplevel.create window in
  Wm.title_set w "omp Font Selection";
  let frame = Frame.create w in
  let listFrame = Frame.create frame in
  let yscroll = Scrollbar.create ~orient:`Vertical listFrame in
  let lstBx = Listbox.create ~width:30 ~height:15 ~selectmode:`Single
  	~yscrollcommand:(Scrollbar.set yscroll)
	listFrame
  in
  Scrollbar.configure ~command:(Listbox.yview lstBx) yscroll;
  Tk.pack ~side:`Right ~fill:`Y [yscroll];
  Tk.pack ~expand:true ~side:`Left ~fill:`Both [lstBx];
  Tk.pack ~expand:true ~fill:`Both [listFrame];
  let sizeFrame = Frame.create frame in
  Tk.pack ~side:`Left [Label.create ~text:"Font size: " ~underline:5
    sizeFrame];
  let spSize = Entry.create ~textvariable:tvSize ~width:3 sizeFrame
  in
  Tk.pack ~side:`Left ~padx:5 [spSize];
  Tk.pack ~side:`Left ~padx:5 [Label.create ~text:"Entry name width: "
    ~underline:6 sizeFrame];
  let spWidth = Entry.create ~textvariable:tvWidth ~width:4 sizeFrame
  in
  Tk.pack ~side: `Left ~padx:5 [spWidth];
  Tk.pack [sizeFrame];
  let wtFr = Frame.create frame in
  Tk.pack ~side:`Top ~anchor:`W [Label.create ~text:"Font weight" wtFr];
  let rbBold = Radiobutton.create ~text:"Bold" ~underline:0 ~value:"bold"
       ~variable:tvWeight wtFr
  in
  let rbMedium = Radiobutton.create ~text:"Medium" ~underline:0 ~value:"medium"
       ~variable:tvWeight wtFr
  in
  Tk.pack ~side:`Left ~padx:5 [rbBold; rbMedium];
  Tk.pack [wtFr];
  let entSample = Entry.create ~width:60 ~textvariable:tvSample frame in
  Tk.pack [entSample];
  let btFr = Frame.create frame in
  let btnView = Button.create ~text:"View selected font" ~underline:0 btFr in
  let btnSave = Button.create ~text:"Save" btFr in
  let btnCancel = Button.create ~text:"Cancel" btFr in
  Tk.pack ~side:`Left [btnView; btnSave; btnCancel];
  Tk.pack [btFr];
  Tk.pack ~expand:true ~fill:`Both [frame];

  let select () =
    let selected = Listbox.curselection lstBx in
    let selectCnt = List.length selected in
    if selectCnt = 1 then (
      match List.nth selected 0 with
        `Num x -> family := List.nth families x
      | _ -> ()
    );
    ()
  in

  let changeFont () =
    select ();
    let size = int_of_string (Textvariable.get tvSize) in
    let weight =
      if Textvariable.get tvWeight = "bold" then `Bold else `Normal
    in
    try
      let font = Font.create ~family:!family ~size ~weight () in
      Entry.configure ~font entSample
    with exn -> ()
  in

  let quit () =
    Tk.destroy w
  in

  let quitev ev = quit () in

  let save ev =
    userfont := !family;
    fontSize := int_of_string (Textvariable.get tvSize);
    fontWeight := if Textvariable.get tvWeight = "bold" then `Bold else `Normal;
    nameWidth := int_of_string (Textvariable.get tvWidth);
    (* omprcSave (); *)
    quit ()
  in

  Textvariable.set tvSize (string_of_int !fontSize);
  Textvariable.set tvWeight (if !fontWeight = `Bold then "bold" else "medium");
  Textvariable.set tvWidth (string_of_int !nameWidth);
  Textvariable.set tvSample "The quick brown fox jumped over the lazy dog. \
  	12345.67890'O'(letter)";
  Listbox.insert ~index:`End ~texts:families lstBx;
  family := !userfont;

  Tk.bind ~events:[`KeyPressDetail "Return"] ~action:save w;
  Tk.bind ~events:[`KeyPressDetail "Escape"] ~action:quitev w;
  Button.configure btnView ~command:changeFont;
  Button.configure btnSave ~command:save;
  Button.configure btnCancel ~command:quit;
  prerr_endline "bind virtual listboxselect";
  let changeFontv ev = changeFont () in
  Tk.bind ~events:[`Virtual "ListboxSelect"] ~action:changeFontv w;
  Radiobutton.configure rbBold ~command:changeFont;
  Radiobutton.configure rbMedium ~command:changeFont;
  prerr_endline "end";
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "s")]
    ~action:(fun ev -> Focus.set spSize) w;
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "n")]
    ~action:(fun ev -> Focus.set spWidth) w;
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "v")]
    ~action:(fun ev -> changeFont ()) w;
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "b")]
    ~action:(fun ev -> Radiobutton.invoke rbBold) w;
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "m")]
    ~action:(fun ev -> Radiobutton.invoke rbMedium) w;
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "f")]
    ~action:(fun ev -> Focus.set lstBx) w;
  
  changeFont ();
  Tkwait.window w

let confEdit () =
  let w = Toplevel.create window in
  Wm.title_set w "omp Configuration Editor";
(*  Style.theme_use "alt";*)
  let frame = Frame.create w in
  let doLabel text column row =
    let lbl = Label.create ~text frame in
    Tk.grid ~column ~row [lbl]
  in
  let doEntry column row default =
    let textvariable = Textvariable.create () in
    Textvariable.set textvariable default;
    let ent = Entry.create ~width:30 ~textvariable frame in
    Tk.grid ~column ~row [ent];
    textvariable
  in
  let doCheck column row default =
    let variable = Textvariable.create () in
    Textvariable.set variable (if default then "1" else "0");
    let chk = Checkbutton.create ~variable frame in
    Tk.grid ~column ~row [chk];
    variable
  in
  let doSpin column row default =
    let textvariable = Textvariable.create () in
    Textvariable.set textvariable default;
    let spbox = Entry.create ~textvariable ~width:5 frame in
    Tk.grid ~column ~row [spbox];
    textvariable
  in
  doLabel "Forgotten Age" 0 4;
  doLabel "Forgotten Minimum Rating" 0 5;
  doLabel "Show Videos" 0 6;
  doLabel "Extensions" 0 7;
  doLabel "Save Interval" 0 8;
  doLabel "Lists to Play" 0 9;
  doLabel "Check Rating" 0 10;
  doLabel "Force New" 0 11;
  doLabel "Update Rating" 0 12;
  doLabel "Check Repeat" 0 13;
  doLabel "Minimum Age" 0 14;
  doLabel "Age Bonus" 0 15;
  doLabel "Skip age check for new entries for # plays" 0 17;
  doLabel "Auto Lock rating after # plays" 0 18;
  let tvForgottenAge = doEntry 1 4 !forgottenAge
  and tvForgottenMin = doSpin 1 5 (string_of_int !forgottenMin)
  and tvShowVideos = doCheck 1 6 !showVideo
  and tvExtensions = doEntry 1 7 (String.listCat !extensions " ")
  and tvSaveInterval = doSpin 1 8 (string_of_int !saveInterval)
  and tvLists = doEntry 1 9 !listsToPlay
  and tvCheckRating = doCheck 1 10 !checkRating
  and tvForceNew = doCheck 1 11 !forceNew
  and tvUpdateRating = doCheck 1 12 !updateRating
  and tvCheckRepeat = doCheck 1 13 !checkRepeat
  and tvMinAge = doEntry 1 14 !minimumAge
  and tvAgeBonus = doEntry 1 15 !ageBonus
  and tvSelMethod = Textvariable.create ()
  in
  Textvariable.set tvSelMethod
    (if !selectMethod = "O" then "Oldest"
     else if !selectMethod = "S" then "Sequential"
     else "Random");
  let tvNewSkipAge = doSpin 1 17 (string_of_int !newSkipAge) in
  let tvAutoLock = doSpin 1 18 (string_of_int !autoLock) in
  let radf = Frame.create frame in
  Tk.pack ~side:`Top ~anchor:`W [Label.create ~text:"Selection Mode" radf];
  let rads = List.map (fun t -> Radiobutton.create ~text:t ~value:t
    ~variable:tvSelMethod radf) ["Random"; "Sequential"; "Oldest"]
  in
  Tk.pack ~side:`Left rads;
  Tk.grid ~column:1 ~row:19 [radf];

  let quit () =
    Tk.destroy w
  in  

  let save () =
    let tryInt var tv =
      try var := int_of_string (Textvariable.get tv) with exn -> ()
    in
(*    let tryFloat var tv =
      try var := float_of_string (Textvariable.get tv) with exn -> ()
    in
*)
    let doBool tv = (Textvariable.get tv = "1") in
    forgottenAge := Textvariable.get tvForgottenAge;
    forgottenAgeFloat := parseAge !forgottenAge !forgottenAgeFloat;
    tryInt forgottenMin tvForgottenMin;
    showVideo := doBool tvShowVideos;
    extensions := G.processExtensions (Textvariable.get tvExtensions);
    tryInt saveInterval tvSaveInterval;
    listsToPlay := Textvariable.get tvLists;
    checkRating := doBool tvCheckRating;
    forceNew := doBool tvForceNew;
    updateRating := doBool tvUpdateRating;
    checkRepeat := doBool tvCheckRepeat;
    minimumAge := Textvariable.get tvMinAge;
    ageBonus := Textvariable.get tvAgeBonus;
    parseMinAge ();
    selectMethod := String.sub (Textvariable.get tvSelMethod) 0 1;
    tryInt newSkipAge tvNewSkipAge;
    tryInt autoLock tvAutoLock;
    omprcSave ();
    quit ()
  in

  let altP ev = selectFont () in

  let buttons = Frame.create frame in
  let btnFont = Button.create ~text:"Playlist configuration"
    ~command:selectFont ~underline:0 buttons in
  let btnSave = Button.create ~text:"Save" ~command:save buttons in
  let btnClose = Button.create ~text:"Close" ~command:quit buttons in
  Tk.pack ~side:`Left [btnFont; btnSave; btnClose];
  Tk.grid ~column:0 ~columnspan:2 ~row:20 [buttons];
  Tk.pack ~expand:true ~fill:`Both [frame];
  Tk.bind ~events:[`Modified([`Alt], `KeyPressDetail "p")]
    ~action:(fun ev -> altP ev) w;
  Tk.bind ~events:[`KeyPressDetail "Escape"] ~action:(fun ev -> quit ()) w;
  Tk.bind ~events:[`KeyPressDetail "Return"] ~action:(fun ev -> save ()) w;
  
  Tkwait.window w
