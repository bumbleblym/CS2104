(**********************************)
(* Lab 2 : Higher-Order Functions *)
(**********************************)
(* We will discuss the initial part of this Lab as Tutorial 
   for the Week of 8 Sept *)

(* This lab assignment must be submitted by 16Sept14 6pm *)

(* 
  Q1: Last via List.fold_Left
  
  Consider the last function below.
  Re-implement it using fold_left.
*)
let last (xs: 'a list) : 'a =
  let rec aux xs prev =
    match xs with
    | [] -> prev
    | x :: ys -> aux ys x in
  match xs with
  | [] -> failwith "no last element"
  | x :: xs -> aux xs x;;

(* replace failwith by your code *)
let last2 (xs: 'a list) : 'a =
  match xs with
  | [] -> failwith "no last element"
  | x :: xs -> List.fold_left (fun _ x -> x) x xs;;

(* 
  Q2 : Sorting
  
  Consider the insertion sort method below.  
  Re-implement the two methods using List.fold_right.
*)
let rec insert x ys =
  match ys with
  | [] -> [x]
  | y :: ys as xs ->
      if x <= y then x :: xs
      else y :: insert x ys;;

let rec sort xs =
  match xs with
  | [] -> []
  | y :: ys -> insert y (sort ys);;

(* replace failwith by your code *)
let rec insert2 x ys =
  match ys with
  | [] -> [x]
  | y :: ys as xs ->
    if x <= y then x :: xs
    else y :: List.fold_right insert2 [x] ys;;

let sort2 xs =
  List.fold_right insert2 xs [];;

(* 
  Q3 : You can compute the average of a list of
  numbers by dividing the sum of the elements by
  the length of the list. Use a single fold_left to
  compute both these values, and then compute
  the average. Throw an exception if the list is empty.
*)

(* replace failwith by your code *)
let average (xs: int list) : float =
  let sum xs =
    match xs with
    | [] -> failwith "empty list"
    | x :: xs -> List.fold_left (fun acc x -> acc + x) x xs
  in float_of_int (sum xs) /. float_of_int (List.length xs);;

(* 
  Q4 : Using Pipeline
  
  You can compute the median of a list of
  integers by sorting the list and computing its
  length, and then finding a middle element in
  the list. If there is an even number of elements,
  you are expected to compute the average of middle two
  elements. You are to use the |> operator as
  below in your median method.
*)
let (|>) (x: 'a) (f: 'a ->'b) : 'b = f x;;

(* your implementation for mid need not use higher-order functions *)
let mid (xs: int list) : float =
  (* pre: input list is sorted *)
  match xs with
  | [] -> failwith "empty list"
  | _ ->
    let len = List.length xs in
    let m1 = float_of_int (List.nth xs ((len - 1) / 2)) in
    let m2 = float_of_int (List.nth xs (len / 2)) in
    m1 +. (m2 -. m1) /. 2.;;

let median xs =
  xs
  |> sort
  |> mid;;

(* 
  Q5 : Higher-Order functions for Trees
  
  You have designed a new tree data structure.
  It is a good practice  to provide a set of higher-order functions.
  
  (i) Based on your understanding of List.map, implement
      a corresponding version for the map_tree function. 

  (ii) Similarly, based on your understanding of List9.fold_right, implement
       a corresponding version for the fold_tree function. 
    
  Some examples of their uses are given below. They may be
  used as test cases for your implementation.
   
*)
type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree;;

let t1 = Node (3, Leaf 1, Leaf 2);;
let t2 = Node (4, t1, t1);;
let t3 = Node (5, t2, t1);;

let rec map_tree (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  match t with
  | Leaf v -> Leaf (f v)
  | Node (v, lt, rt) -> Node (f v, lt, rt);;
(* 
   map_tree f (Node a1,Leaf a2,Leaf a3) 
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let rec fold_tree (f1: 'a -> 'b) (f2: 'a -> 'b -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | Leaf v -> f1 v
  | Node (v, lt, rt) -> f2 v (fold_tree f1 f2 lt) (fold_tree f1 f2 rt);;
(* 
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> f2 a2 (f1 a1) (f1 a1)
*)

let t4 = map_tree (fun x -> 2 * x) t3;;
(* expecting a doubled version of t3
   Node (10, Node (8, Node (6, Leaf 2, Leaf 4), Node (6, Leaf 2, Leaf 4)),
     Node (6, Leaf 2, Leaf 4))
*)
fold_tree (fun x -> x) (fun a b c -> a + b + c) t3;;
(* expecting 27 *)
fold_tree (fun x -> [x]) (fun a b c -> b @ a :: c) t1;;
(* in-order traversal [1; 3; 2] *)
fold_tree (fun x -> [x]) (fun a b c -> a :: b @ c) t1;;
(* pre-order traversal [3; 1; 3] *)

(*
 
  Q6 : Map in terms of Fold
  
  Now that you have an implementation of fold_tree.
  Show how map_tree can be implemented in terms of folr_tree.
   
*)
let map_tree2 (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  let f1 v = Leaf (f v) in
  let f2 v lt rt = Node (f v, lt, rt) in
  fold_tree f1 f2 t;;

(* please remember to add ;; above which can optionally be used to *)
(* separate global declarations but must be present if *)
(* you have a global expression after your declaration like below *)
map_tree2 (fun x -> 2 * x) t2;;
(* Expecting:  Node (8, Node (6, Leaf 2, Leaf 4), Node (6, Leaf 2, Leaf 4)) *)

(*

  Q7: Pretty printers.
  
  Consider the binary tree defined earlier. 
  
  You have been given a higher-order printer which prints the tree in a pre-fix form.
  As an example, the tree t2 would be printed as:

Node 4
Node 3
Leaf 1
Leaf 2
Node 3
Leaf 1
Leaf 2

  (i) pretty printer
  This above printing is however less readable and you are asked to provide
  a neater printer that would provide space indentation to represent
  the depth of each subtrees.
    
  Implement pr_tree2, so that it would provide such space indentation
  for each new level of the subtrees, as illustrated below:

Node 4
 Node 3
  Leaf 1
  Leaf 2
 Node 3
  Leaf 1
  Leaf 2
  
  (i) infix printer.
  One may prefer a tree printer that is presented in an infix manner.
  Write a new pr_tree_infix method that would allow your binary tree to be printed
  in an infix order. The output for t2 example is illustrated below:
  
  Leaf 1
 Node 3
  Leaf 2
Node 4
  Leaf 1
 Node 3
  Leaf 2

*)
let pr_tree (pr: 'a -> string) (xs: 'a tree) : string =
  let rec aux xs =
    match xs with
    | Leaf e -> "Leaf " ^ pr e ^ "\n"
    | Node (e, lt, rt) ->
        "Node " ^ pr e ^ "\n"
        ^ aux lt ^ aux rt
  in aux xs;;

(* please change failwith .. to your implementation *)
let rec fold_tree_acc
    (f1: 'a -> 'c -> 'b)
    (f2: 'a -> 'b -> 'b -> 'c -> 'b)
    (f3 : 'c -> 'c)
    (acc : 'c)
    (t: 'a tree) : 'b =
  match t with
  | Leaf v -> f1 v acc
  | Node (v, lt, rt) ->
      f2 v
        (fold_tree_acc f1 f2 f3 (f3 acc) lt)
        (fold_tree_acc f1 f2 f3 (f3 acc) rt)
        acc;;

let pr_tree2 (pr: 'a -> string) (xs: 'a tree) : string =
  let f1 v acc = acc ^ "Leaf " ^ pr v ^ "\n" in
  let f2 v lt rt acc = acc ^ "Node " ^ pr v ^ "\n" ^ lt ^ rt in
  let f3 acc = acc ^ "  " in
  fold_tree_acc f1 f2 f3 "" xs;;

(* please change failwith .. to your implementation *)
let pr_tree_infix (pr: 'a -> string) (xs: 'a tree) : string =
  let f1 v acc = acc ^ "Leaf " ^ pr v ^ "\n" in
  let f2 v lt rt acc = lt ^ acc ^ "Node " ^ pr v ^ "\n" ^ rt in
  let f3 acc = acc ^ "  " in
  fold_tree_acc f1 f2 f3 "" xs;;

let test t =
  print_endline (pr_tree string_of_int t);
  print_endline (pr_tree2 string_of_int t);
  print_endline (pr_tree_infix string_of_int t);;

(*

  Q8: Numbered List. 
  
  You have been previously given a printer for lists.
  
  pr_list2 pr_id ls ==>
  - : string = "[This; is; a; numbered; list]"
  
  You have been asked to write a list printer that would number each
  element of its list. Your new function pr_list_num  should result in
  the following:
  
  pr_list_num "; " (fun x->x) ls ==>
  - : string = "[(1)This; (2)is; (3)a; (4)numbered; (5)list]"  

  You may use the add_num method below which adds a number to each element
  of its list. You should make use of the |> operator and write it
  in a similar style as pr_list2.
*)
let pr_list2 (pr: 'a -> string) (xs: 'a list) : string =
  "["
  ^ (xs |> List.map pr |> String.concat "; ")
  ^ "]";;

let add_num (xs: 'a list) : (int * 'a) list =
  let rec aux xs n =
    match xs with
    | [] -> []
    | x :: xs -> (n, x) :: aux xs (n + 1)
  in aux xs 1;;

let ls = ["This"; "is"; "a"; "numbered"; "list"];;

let pr_id x = x;;

let pr_list_num (sep: string) (pr: 'a -> string) (xs: 'a list) : string =
  xs
  |> List.mapi (fun i s -> "(" ^ string_of_int (i + 1) ^ ")" ^ pr s)
  |> String.concat sep
  |> fun s -> "[" ^ s ^ "]";;

let test_num sep pr xs =
  let s = pr_list_num sep pr xs in
  print_endline s;;

(*
  Q9. Higher-order wrappers.
  
     These are great for modifying the tracing and monitoring
     our methods, and could even be user to alter our methods' behaviour.
     
     One use of them is to help us perform method call tracing, so
     as to determine the correctness of our method. A simple tracer for
     methods is given below which was also applied recursively to the fib method.
    
     An example use of tracer is shown below which traced all calls to fib1 4 
     before returning a final result 5.
    
# fib1 4;;
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 1 => 1
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : int = 5

     Your task is to implement a more selective tracer_test method that
     takes a predicate, and would only output a call tracing if the predicate
     holds with the input parameter. An example of its use is below:
    
     let rec fib3 n = 
       trace_test "fib" (fun x -> x>1) string_of_int string_of_int aux n
    
     which only output a trace if input x>1. This would rule out printing
     the base-cases of x<=1 leading to tracing below:

# fib3 4;;
fib 2 => 2
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : int = 5

     Please modify tracer_test to achieve a more selective tracing of
     the calls. 

*)

let wrapper (pre:'a->'v) (post:'v->'b->unit)
    (post_exc: 'v->exn->unit) 
    (f:'a->'b) (x:'a) : 'b =
  let v = pre x in
  try 
    let r = f x in
    let () = post v r in
    r
  with e -> 
    let () = post_exc v e in
    raise e;;

let out_print x = print_endline x

(* function tracing *)
let tracer fn_str pr_arg pr_res f x =
   wrapper 
    (fun x -> fn_str^" "^(pr_arg x))
    (fun v r -> out_print (v^" => "^(pr_res r)))
    (fun v e -> out_print (v^" => Exception"))
    f x

(* non-recursive tracing of just the first call *)
let rec fib n = 
    if n<=1 then 1
    else fib (n-1)+(fib(n-2));;
let fib1 n = 
    tracer "fib" string_of_int string_of_int fib n

(* recursive tracing of all calls *)
let rec fib2 n = 
    tracer "fib" string_of_int string_of_int aux n
and aux n = 
    if n<=1 then 1
    else fib2 (n-1)+(fib2(n-2));;

(* selective function tracing *)      
let trace_test (fn_str:string) (pr_test:'a->bool) 
  (pr_arg:'a->string) (pr_res:'b->string) (f:'a->'b) (x:'a) : 'b =
  failwith "use wrapper to implement a selective function tracing"

(* selective tracing of calls *)
let rec fib3 n = 
    trace_test "fib" (fun x -> x>1) string_of_int string_of_int aux n
and aux n = 
    if n<=1 then 1
    else fib3 (n-1)+(fib3(n-2));;
