open Tas;;
open Printf;;

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

(* Tests *)
let k1 () = 1;;
let k2 () = 2;;
let k3 () = 1;;
let g = algorithm_1 [|k1;k2;k3|];;
Array.iter (fun t -> Array.iter (fun b -> if b then print_string "1 - " else print_string "0 - ") t; print_newline ()) g;;


(* Barabasi-Albert algorithm *)
(* let full_graph n m = *)
(*   let g = Array.make_matrix n n false in *)
(*   for i = 0 to (m-1) do *)
(*     for j = 0 to (m-1) do *)
(*       if i <> j then g.(i).(j) <- true *)
(*     done; *)
(*   done; *)
(*   (g : graphe) *)
(* ;; *)
(* *)
(* module Int_set = Set.Make(Int);; *)
(**)
(* let pick_distincts tab m = *)
(*   let n = Array.length tab in *)
(*   let rec aux s = function *)
(*     | k when k=m -> s *)
(*     | _ -> let i = Random.int n in *)
(*       let ns = Int_set.add tab.(i) s in aux ns (Int_set.cardinal ns) *)
(*   in aux (Int_set.empty) 0 *)
(* ;; *)
(* *)
(* let algorithm_barabasi_albert n m m0 = *)
(*   let g = full_graph n m0 and deg = Array.init (m0 * (m0-1)) (fun i -> i / m0) in *)
(*   let rec aux deg = function *)
(*     | k when k = n -> g *)
(*     | k -> let nodes = pick_distincts deg m and nm = Array.make m k in *)
(*     Int_set.iter (fun i -> g.(k).(i) <- true; g.(i).(k) <- true) nodes; *)
(*       aux (Array.concat [deg; Array.of_seq (Int_set.to_seq nodes); nm]) (k+1) *)
(*   in aux deg m0 *)
(* ;; *)
(* *)
(* let print_graph (g : graphe) = *)
(*   let n = Array.length g in *)
(*   for i = 0 to (n-1) do *)
(*     printf "["; *)
(*     for j = 0 to (n-1) do *)
(*       if g.(i).(j) then printf "%d, " j *)
(*     done; *)
(*     printf "],\n" *)
(*   done; *)
(* ;; *)
let print_graph_degree (g : graphe) =
  let n = Array.length g in
  for i = 0 to (n-1) do
    let r = ref 0 in
    for j = 0 to (n-1) do
      if g.(i).(j) then incr r
    done;
    printf "%d," !r
  done;
;;


(* Optimized algorithm *)
let algorithm_3 (k : (unit -> int) array) =
  let n = Array.length k and degs = Array.map (fun k_i -> k_i ()) k in
  let g = Array.make_matrix n n false in
  let nodes_tab = Array.init n (fun i -> {deg= degs.(i); value= i}) in
  let f1 = cree_tas nodes_tab and f2 = {content= Array.make n {deg= 0; value=0}; length= 0} in
  while f1.length > 0 do
    let node = retrait f1 and connected = ref [] in
    while node.deg > 0 && f2.length > 0 do
      let connect = retrait f2 in
      g.(node.value).(connect.value) <- true;
      g.(connect.value).(node.value) <- true;
      node.deg <- node.deg - 1;
      connect.deg <- connect.deg - 1;
      if connect.deg > 0 then connected := connect::!connected
    done;
    List.iter (fun i -> ajout i f2) !connected;
    if node.deg > 0 then ajout node f2
  done;
  if f2.length > 0 then printf "Graphe invalid: %d" f2.length;
  (g : graphe)
;;

let k () = 1+(Random.int 998);;
let tab_k = Array.make 1000 k;;
print_graph_degree (algorithm_3 tab_k);;
