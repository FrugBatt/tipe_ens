type node = {mutable deg: int; value: int};;
type tas = {content: node array; mutable length: int};;

exception Tas_plein;;
exception Tas_vide;;

let swap t i j =
  let x = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- x
;;

let monte t k =
  let rec aux = function
    | 0 -> ()
    | i -> let j = (i-1)/2 in
      if t.(i).deg > t.(j).deg then (swap t i j; aux j)
  in aux k
;;

let descend t n k =
  let rec aux = function
    | i when 2*i+2 > n -> ()
    | i -> let j = if 2*i+2 = n || t.(2*i+1).deg > t.(2*i+2).deg then 2*i+1 else 2*i+2 in
      if t.(i).deg < t.(j).deg then (swap t i j; aux j)
  in aux k
;;

let ajout n t =
  if t.length = Array.length t.content then raise Tas_plein;
  t.content.(t.length) <- n;
  monte t.content t.length;
  t.length <- t.length + 1
;;

let retrait t =
  if t.length = 0 then raise Tas_vide;
  let min = t.content.(0) in
  t.length <- t.length - 1;
  t.content.(0) <- t.content.(t.length);
  descend t.content t.length 0;
  min
;;


let cree_tas tab =
  let n = Array.length tab in
  let t = {content= tab; length= n} in
  for k = n/2-1 downto 0 do
    descend tab n k
  done;
  t
;;
