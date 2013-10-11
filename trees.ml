
type inttree = Empty | Node of int * inttree * inttree

let divAv (num, tot) = ( tot / num )

(* use this function in fromList *)
let rec insert t i =
  match t with
      Empty -> Node(i,Empty,Empty)
    | Node(j,l,r) -> 
      if i=j 
      then t 
      else if i < j 
      then Node(j,insert l i,r)
      else Node(j,l,insert r i)

(* no need for this function; it is just an example *)
let rec member t i =
  match t with
      Empty -> false
    | Node(j,l,r) -> i=j || (i < j && member l i) || member r i

(* put fromList, sum1, prod1, avg1, map and negateAll here *)
let rec fromList intlist =
  match intlist with
    [] -> Empty
    | h::l -> insert (fromList l) h

let rec sum1 tree = 
  match tree with
    Empty -> 0
    | Node(j,l,r) -> j + sum1 l + sum1 r

let rec prod1 tree = 
  match tree with
    Empty -> 1
    | Node(j,l,r) -> j * prod1 l * prod1 r

let avg1 tree = 
    let rec f curtree number total = 
    match curtree with
      Empty -> (0,0)
      | Node(j,l,r) -> 
      match f l (number) (total) , f r (number) (total) with
          (leftN,leftT) , (rightN,rightT) -> 
          (leftN + rightN + 1,rightT + leftT + j)
    in divAv ( f tree 0 0)

let rec map f tree  =
   match tree with
    Empty -> Empty
    | Node(j,l,r) -> Node(f j, map f l, map f r)

let negateAll tree = map (fun x -> -x) tree

let rec fold f a t =
  match t with
      Empty -> a
    | Node(j,l,r) -> fold f (fold f (f a j) l) r

(* put sum2, prod2, and avg2 here *)

let rec sum2 tree = 
  fold (fun init x -> init+x) 0 tree

let rec prod2 tree = 
  fold (fun init x -> init*x) 1 tree

let avg2 tree = 
      divAv (fold (fun init x -> ((fst init) + 1,  (snd init) + x)) (0,0) tree)

type 'a iterator = Nomore | More of 'a * (unit -> 'a iterator)

let rec iter t =
  let rec f t k =
    match t with
	Empty -> k ()
    | Node(j,l,r) -> More(j, fun () -> f l (fun () -> f r k))
  in f t (fun () -> Nomore)

let rec sum3 tree = 
  let rec f iterator sum =
    match iterator with
      Nomore -> sum
      | More(j, rest) -> f (rest ()) (sum + j)
    in f (iter tree) 0

let rec prod3 tree = 
  let rec f iterator prod = 
    match iterator with
      Nomore -> prod
      | More(0, rest) -> 0
      | More(j, rest) -> f (rest ()) (prod * j)
    in f (iter tree) 1


let rec avg3 tree = 
    let rec f iterator number total = 
    match iterator with
      Nomore -> (number,total)
      | More(j, rest) -> f (rest ()) (number + 1) (total + j) 
    in divAv ( f (iter tree) 0 0)

(* challenge problem: put optionToException and exceptionToOption here *)

(* a little testing -- commented out since the functions do not exist yet *)
let rec print_space n =
  if n > 0 then (
    print_string "  ";
    print_space (n-1)
  )
(* print function taken from class piazza site *)
let print_tree t =
  let rec print_tree_helper t2 d =
    match t2 with
      Empty -> print_string "";
      | Node(num,l,r) -> (
          print_tree_helper r (d + 1);
          print_space d;
          print_int num;
          print_string "\n";
          print_tree_helper l (d + 1)
        )
  in
    match t with
      Empty -> print_string "Empty"
      | Node(num,l,r) -> print_tree_helper t 0

let tr = fromList [0;1;2;3;4;5;6;7;8;9;9;9;1] (* repeats get removed *)
let print_ans f t = print_string (string_of_int (f t)); print_string "\n"
let _ = print_ans sum1 tr
let _ = print_ans prod1 tr
let _ = print_ans avg1 tr
let _ = print_tree tr
let _ = print_tree (map (fun x -> x+x) tr)
let _ = print_tree (negateAll tr)
let _ = print_ans sum2 tr
let _ = print_ans prod2 tr
let _ = print_ans avg2 tr
let _ = print_ans sum3 tr
let _ = print_ans prod3 tr
let _ = print_ans avg3 tr

