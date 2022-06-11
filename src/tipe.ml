(* On implémente un graphe à l'aide de sa matrice d'adjacence *)
type graphe = bool array array;;


let algorithm_1 (k : (unit -> int) array) =
  let n = Array.length k and b = ref false in
  let g = Array.make_matrix n n false in
  while not !b do
    let degs = Array.map (fun k_i -> k_i ()) k in
    try
      for i = 0 to (n-1) do
        let j = ref (i+1) in
        while (degs.(i) <> 0) && (!j < n) do
          if degs.(!j) <> 0 then begin
            degs.(!j) <- degs.(!j) - 1;
            degs.(i) <- degs.(i) - 1;
            g.(i).(!j) <- true;
            g.(!j).(i) <- true
          end;
          incr j
        done;
        if degs.(i) <> 0 then raise Exit
      done;
      b := true
    with Exit -> ()
  done;
  (g : graphe)
;;

let k1 () = 1;;
let k2 () = 2;;
let k3 () = 1;;
let g = algorithm_1 [|k1;k2;k3|];;
Array.iter (fun t -> Array.iter (fun b -> if b then print_string "1 - " else print_string "0 - ") t; print_newline ()) g;;
