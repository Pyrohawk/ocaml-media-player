(* vlc.ml - vlc interface
   Charles Emmett Hawkins
   2012-12-02
*)

open Qlib

let try_int s = try int_of_string s with exn -> 0

class cVLC =
  let (ich, och) = Unix.open_process "vlc -q -I rc"
  in
  object(self)

  val mutable mixerAvail = true
  val mutable volMst = 0
  val mutable maxMst = 512
  val mutable result = ""
  val mutable busy = false

  initializer
    Unix.set_nonblock (Unix.descr_of_in_channel ich);
    Unix.sleep 2;  (* Give vlc time to load *)
    self#get_vol
    
  method private get_vol = volMst <- self#get_int "volume"    
  
  method is_available = mixerAvail
  method getMaster = (volMst, maxMst)

  (** "+" increments, "-" decrements *)
  method adjust_volume amt =
    let newvol = volMst + amt in
    volMst <- max 0 (min newvol maxMst);
    let cmd = Printf.sprintf "volume %d" volMst in
    self#wait;
    self#vlc_cmd cmd;
    self#wait;
    self#get_vol

  method private wait = (*while busy do QDate.usleep 0.101 done*) ()

  method play path =
    let cmd = Printf.sprintf "add %s" path in
    Unix.sleep 2; (* Trying to fix problem with ubuntu screwed up sound *)
    self#wait;
    self#vlc_cmd cmd
    
  method stop = self#wait; self#vlc_cmd "stop"
  method restart = self#wait; self#vlc_cmd "seek 0"
  method pause = self#wait; self#vlc_cmd "pause"
  method video_off = self#wait; self#vlc_cmd "vtrack -1"
  method video_on = self#wait; self#vlc_cmd "vtrack 0"
  method get_time = self#wait; self#get_int "get_time"    
  method get_length = self#wait; self#get_int "get_length"
  
  (** amt is number of seconds to seek forward or backward if negative *)
  method jump amt =
    let pos = self#get_time
    and len = self#get_length
    in
    let newpos = pos + amt in
    let newpos = max 0 (min len newpos) in
    let cmd = Printf.sprintf "seek %d" newpos in
    self#wait;
    self#vlc_cmd cmd
  
  method is_playing =
    self#wait; 
    self#vlc_cmd "is_playing";
    let b = String.field result '\n' 0 in
(*Printf.printf "is_playing='%s'\n%!" b;*)
    b = "1"
    
  method get_int cmd =
    self#wait; 
    self#vlc_cmd cmd;
(*Printf.printf "field 0='%s'\n%!" (String.field result '\n' 0);
Printf.printf "field 1='%s'\n%!" (String.field result '\n' 1);
*)
    try_int (String.field result '\n' 0)

  method private poll =
    let (inlist, _, _) =
      Unix.select [ Unix.descr_of_in_channel ich ] [] [] 0.1
    in
    inlist <> []
    
  method private read =
    let ch = Bytes.create 1 in
    let buf = Buffer.create 11 in
    let n = ref 1 in
    if self#poll then begin
      while (!n > 0) do
        n := (try input ich ch 0 1 with exn -> 0);
        if !n > 0 then Buffer.add_bytes buf ch
      done
    end;
    Buffer.contents buf
    
  method private put s =
    output_string och (s ^ "\n");
    flush och
    
  method private vlc_cmd cmd =
    busy <- true;
    (* Make sure input buffer is empty *)
    ignore (self#read);
(*Printf.printf "vlc_cmd(%s)\n%!" cmd;*)
    self#put cmd;
    result <- String.replace self#read "\r" "";
(*Printf.printf "vlc_cmd response: %s\n%!" result;*)
    busy <- false

   
  method quit =
    self#vlc_cmd "quit"
    
end
