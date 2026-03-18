(* If you have ocaml installed, you can run this as "ocaml agebonus.ml".
   If you want to compile it, use "ocamlc -o agebonus agebonus.ml"
   or "ocamlopt -o agebonus agebonus.ml" *)

let step = tan (1.) /. 99. in
for i = 1 to 100 do
  (* The 3 exponential exagerates the curve, IOW, flattening it more
     at the extremes *)
  let n = 100. *. (atan (step *. (100. -. (float i))) ** 3.) in
  Printf.printf "%.2f\n" n
done

