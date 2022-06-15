open Tas;;

let rec partition = function
  | [] -> ([],[])
  | [a] -> ([a],[])
  | a::b::t -> let (l1,l2) = partition t in (a::l1,b::l2)
;;

let rec fusion l1 l2 = match (l1,l2) with
  | ([],_) -> l2
  | (_,[]) -> l1
  | (h1::t1,h2::t2) ->
      if h1 > h2 then h1::(fusion t1 l2)
      else h2::(fusion l1 t2)
;;

let rec fusion_node l1 l2 = match (l1,l2) with
  | ([],_) -> l2
  | (_,[]) -> l1
  | (h1::t1,h2::t2) ->
      if h1.deg > h2.deg then h1::(fusion_node t1 l2)
      else h2::(fusion l1 t2)
;;

let rec tri_fusion = function
  | [] -> []
  | [a] -> [a]
  | l -> let (l1,l2) = partition l in fusion (tri_fusion l1) (tri_fusion l2)
;;
