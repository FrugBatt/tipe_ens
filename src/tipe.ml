open Tas;;
open Printf;;
open Tri;;

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


let take_last tab i =
  let n = Array.length tab in
  if i < 0 || n = 0 then 0
  else if i >= n then tab.(n-1)
  else tab.(i)
;;

(* Erdos Gallai check *)
let erdos_gallai (k : int array) =
  let n = Array.length k and tab = Array.of_list (tri_fusion (Array.to_list k)) in
  let up_sum = Array.make (n+1) 0 and down_sum = Array.make (n+2) [||] in
  for i = 1 to n do up_sum.(i) <- up_sum.(i-1) + tab.(i-1) done;
  for p = n downto 1 do
    down_sum.(p) <- Array.make (tab.(p-1)+1) 0;
    for l = 0 to (tab.(p-1)) do
      down_sum.(p).(l) <- l + (take_last down_sum.(p+1) l)
    done;
  done;
  let i = ref 1 in
  while !i <= n && (up_sum.(!i) <= !i*(!i-1) + (take_last down_sum.(!i) !i)) do incr i done;
  (!i = n+1)
;;

(* let algorithm_3 (k : (unit -> int) array) = *)
(*   let n = Array.length k and degs = Array.map (fun k_i -> k_i ()) k in *)
(*   if erdos_gallai degs then printf "Graph OK\n" else printf "Graph not OK\n"; *)
(*   let g = Array.make_matrix n n false in *)
(*   let nodes_tab = Array.init n (fun i -> {deg= degs.(i); value= i}) in *)
(*   let f1 = cree_tas nodes_tab and f2 = {content= Array.make n {deg= 0; value=0}; length= 0} in *)
(*   while f1.length > 0 do *)
(*     let node = retrait f1 and connected = ref [] in *)
(*     while node.deg > 0 && f2.length > 0 do *)
(*       let connect = retrait f2 in *)
(*       g.(node.value).(connect.value) <- true; *)
(*       g.(connect.value).(node.value) <- true; *)
(*       node.deg <- node.deg - 1; *)
(*       connect.deg <- connect.deg - 1; *)
(*       if connect.deg > 0 then connected := connect::!connected *)
(*     done; *)
(*     List.iter (fun i -> ajout i f2) !connected; *)
(*     if node.deg > 0 then ajout node f2 *)
(*   done; *)
(*   if f2.length > 0 then printf "Graphe invalid: %d\n" f2.length; *)
(*   (g : graphe) *)
(* ;; *)


(* let print_tab tab = *)
(*   let n = Array.length tab in *)
(*   for i = 0 to (n-1) do *)
(*     printf "%d:%d -- " tab.(i).value tab.(i).deg *)
(*   done; *)
(*   printf "\n" *)
(* ;; *)

(* Optimized algorithm *)
let algorithm_3 (k : (unit -> int) array) =
  let n = Array.length k and degs = Array.map (fun k_i -> k_i ()) k in
  if erdos_gallai degs then printf "Graph OK\n" else printf "Graph not OK\n";
  let g = Array.make_matrix n n false in
  let nodes_tab = Array.init n (fun i -> {deg= degs.(i); value= i}) in
  let f1 = cree_tas nodes_tab in
  while f1.length > 0 do
    let node = retrait f1 and connection = ref false and connected = ref [] in
    while node.deg > 0 && f1.length > 0 && not !connection do
      let connect = retrait f1 in
      if not g.(node.value).(connect.value) then begin
        g.(node.value).(connect.value) <- true;
        g.(connect.value).(node.value) <- true;
        node.deg <- node.deg - 1;
        connect.deg <- connect.deg - 1;
        connection := true;
        (* printf "Connection %d:%d -- %d:%d\n" node.value node.deg connect.value connect.deg; *)
      end;
      if connect.deg > 0 then connected := connect::!connected
    done;
    (* if node.deg > 0 && f1.length = 0 then print_tab f1.content; printf "Length : %d\n" (List.length !connected); *)
    if node.deg > 0 && f1.length = 0 then failwith "Graph invalid";
    if node.deg > 0 then connected := node::!connected;
    List.iter (fun i -> ajout i f1) !connected;
  done;
  (g : graphe)
;;


(* let k () = (Random.int 500);; *)
(* let tab_k = Array.make 500 k;; *)
(* let k () = 7;; *)
(* let tab_k = Array.make 6 k;; *)
(* let t = [| 11 ;12 ;13 ;8 ;10 ;4 ;3 ;5 ;3 ;6 ;3 ;5 ;4 ;5 ;3 ;3 ;3 ;3 ;3 ;3|];; *)
(* let t = [| 5 ;8 ;8 ;6 ;6 ;3 ;4 ;3 ;4 ;3 |];; *)
(* let t = [| 3 ;5 ;4 ;2 ;2 ;2 |];; *)
let t = [| 85 ;50 ;54 ;54 ;36 ;69 ;17 ;22 ;15 ;54 ;13 ;9 ;15 ;10 ;22 ;8 ;31 ;11 ;9 ;24 ;7 ;11 ;17 ;13 ;9 ;18 ;21 ;14 ;15 ;17 ;15 ;10 ;11 ;11 ;9 ;7 ;23 ;9 ;15 ;14 ;16 ;10 ;7 ;5 ;10 ;6 ;11 ;7 ;13 ;16 ;5 ;3 ;4 ;5 ;11 ;3 ;9 ;7 ;15 ;13 ;8 ;9 ;7 ;5 ;6 ;9 ;8 ;5 ;7 ;4 ;24 ;11 ;6 ;13 ;4 ;7 ;8 ;10 ;8 ;4 ;8 ;5 ;12 ;14 ;7 ;8 ;8 ;5 ;8 ;4 ;5 ;5 ;8 ;9 ;4 ;13 ;4 ;4 ;9 ;11 ;4 ;5 ;11 ;8 ;5 ;5 ;7 ;6 ;3 ;7 ;11 ;7 ;8 ;6 ;9 ;5 ;9 ;7 ;3 ;4 ;3 ;5 ;7 ;4 ;6 ;3 ;5 ;6 ;6 ;7 ;11 ;7 ;7 ;5 ;4 ;8 ;8 ;7 ;4 ;6 ;4 ;4 ;3 ;4 ;6 ;7 ;3 ;5 ;5 ;8 ;7 ;4 ;6 ;3 ;7 ;5 ;4 ;4 ;7 ;4 ;5 ;5 ;6 ;7 ;10 ;3 ;5 ;4 ;3 ;5 ;6 ;4 ;6 ;6 ;3 ;4 ;5 ;5 ;5 ;4 ;5 ;4 ;4 ;3 ;5 ;6 ;5 ;4 ;5 ;4 ;4 ;3 ;9 ;7 ;4 ;8 ;5 ;3 ;5 ;11 ;5 ;3 ;4 ;4 ;4 ;5 ;3 ;5 ;5 ;5 ;4 ;4 ;3 ;5 ;5 ;4 ;5 ;3 ;3 ;3 ;4 ;3 ;6 ;3 ;3 ;6 ;6 ;4 ;4 ;3 ;8 ;5 ;6 ;3 ;3 ;3 ;3 ;3 ;4 ;3 ;3 ;5 ;4 ;6 ;3 ;6 ;4 ;7 ;4 ;5 ;4 ;4 ;3 ;5 ;3 ;6 ;3 ;5 ;3 ;3 ;4 ;3 ;3 ;4 ;5 ;6 ;4 ;4 ;5 ;5 ;3 ;4 ;4 ;5 ;4 ;4 ;3 ;3 ;3 ;4 ;3 ;3 ;3 ;7 ;3 ;4 ;5 ;3 ;3 ;3 ;5 ;5 ;3 ;3 ;3 ;4 ;4 ;3 ;3 ;4 ;6 ;3 ;4 ;4 ;3 ;4 ;3 ;3 ;4 ;4 ;3 ;5 ;3 ;4 ;3 ;4 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;3 ;4 ;3 ;4 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;4 ;3 ;3 ;3 ;3 ;4 ;3 ;4 ;4 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;7 ;3 ;3 ;3 ;4 ;4 ;3 ;3 ;5 ;3 ;3 ;3 ;4 ;4 ;3 ;4 ;4 ;3 ;5 ;3 ;4 ;4 ;4 ;3 ;4 ;4 ;3 ;5 ;3 ;5 ;4 ;3 ;3 ;3 ;3 ;4 ;4 ;3 ;3 ;3 ;4 ;4 ;3 ;3 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;4 ;4 ;4 ;4 ;3 ;3 ;4 ;3 ;4 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;4 ;4 ;3 ;3 ;3 ;4 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;4 ;3 ;3 ;5 ;5 ;3 ;4 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 ;3 |];;
let n = Array.length t;;
let tab_k = Array.init n (fun i -> (fun () -> t.(i)));;
let g = algorithm_3 tab_k;;
print_graph_degree g;;
