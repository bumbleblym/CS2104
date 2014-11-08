(* 
   Lab Assignment 1 : Declarative OCaml Examples
   
   Week of 25th August 2014

   Everyone should submit this assignment by 8pm 2Sept2014
   In case of problem, please consult me/tutors either
   during tutorial or after lecture.
*)


(* 
  Q1 : Write a recursive function that would return the
       last element of the list. In case of empty list,
       throw a Failure exception.

       What is the polymorphic type of this function?
*)
let rec last (xs: 'a list) : 'a =
  match xs with
  | [] -> failwith "empty list"
  | [x] -> x
  | _ :: xs -> last xs;;

(* 'a list -> 'a *)

(* 
  Q2 : Change the last function to one with the following
       type: 'a list -> 'a option
       This function should return Some v, where v is the
       last element of the list. If the list is empty, you
       should return None.
*)
let rec last2 (xs: 'a list) : 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last2 xs;;

(* 
  Q3 : Write a recursive function that would return the
       last two elements of the lists as a pair of values.
       In case you have less than two elements, throw a Failure exception.

       What is the polymorphic type of this function?
*)
let last_two (xs:'a list) : 'a * 'a =
  failwith "last_two not implemented yet"


(* 
  Q4 : Write a recursive function to sort a list of numbers
       using the insertion sort method.

       For your convenience, we have provided an
       insert procedure.
       (i) can you improve the insert method to
           avoid constructing (y::ys) in the base case?
           (Hint : use the as-pattern notation)
      (ii) implement a recursive sort method
*)

let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys -> 
          if x<=y then x::y::ys
          else y::(insert x ys)
let sort xs=
  failwith "sort method based on insertion sort"

(* 
  Q4 : Consider a uprim type to capture either 
       integer, float or a string value.

       You can build a list of mixed type using
       it, and can perform List.rev and List.length
       using it.

       Compute the sum of mixed list using the value_of_mix
       function.
*)
type uprim = I of int | F of float | S of string ;;

let mix_ls = [I 3; F 4.3; S "hello"; I 4];;

print_endline ("mix_ls has length "^(string_of_int (List.length mix_ls)));;
List.rev  mix_ls;;

let value_of_mix up =
  match up with
    | I v -> v
    | F v -> (int_of_float v) (* truncates the float value *)
    | S s -> (String.length s) (* length of string *)

let sum_of_mix_list (ms: uprim list) : int =
  failwith "sum_of_mix_list to be implemented"


(* 
  Q5 : Let us define uprim using the basic sum type instead,
       and write functions that are isormoprhic to those
       found in Q4.
*)
type ('a,'b) sum = L of 'a | R of 'b;;

type uprim2 = (int,(float,string) sum) sum;;

let mk_I (v:int) = L v  (* makes an integer value *)
let mk_F (f:float) = R (L f) (* makes a float value *)
let mk_S (s:string) = R (R s) (* makes a string value *)

let mix_ls2 = [mk_I 3; mk_F 4.3; mk_S "hello"; mk_I 4];;

print_endline ("mix_ls2 has length "^(string_of_int (List.length mix_ls2)));;
List.rev  mix_ls2;;

let value_of_mix2 up =
  match up with
    | L v -> v
    | R (L v) -> (int_of_float v) (* truncates the float value *)
    | R (R s) -> (String.length s) (* length of string *)

let sum_of_mix_list2 (ms: uprim2 list) : int =
  failwith "sum_of_mix_list2 to be implemented"


(* 
  Q6 : Consider a polymorphic tree.

       Write a function that will return the largest value in
       the tree. You may use the max function.
*)
type 'aa btree = Leaf of 'aa | Node of 'aa * ('aa btree) * ('aa btree) ;;
let t1 = Leaf 3;;
let t2 = Node(4,t1,t1);;
let t2 = Node(6,t2,t1);;

let rec max_tree (t: int btree) : int =
  failwith "max_tree to be implemented"


(* 
  Q7 : Below is a function that will flatten a tree into a list
       by traversing the tree in an infix-order.

       Write another function that will flatten a tree 
       based on pre-fix traversal.
*)
let rec flatten_infix (t: 'a btree) : 'a list =
  match t with
    | Leaf v -> [v]
    | Node(v,lt,rt) -> (flatten_infix lt)@[v]@(flatten_infix rt)

let flatten_prefix (t: 'a btree) : 'a list =
  let rec aux t =
    match t with
      | Leaf v -> [v]
      | Node(v,lt,rt) -> failwith "max_tree to be implemented"
  in aux t

(* 
  Q8 : The power function takes two arguments x n so as
       to return x^n.


       An expected precondition is that n>=0
       Write an assertion statement to ensure that this pre-condition
       will always be met.

       What happens to your function if you had used
       a negative n value?
*)
let rec power (x:int) (n:int) : int =
  if n==0 then 1
  else n * (power x (n-1))


(* 
  Q9 : 

       The above code below merely expresses the fact that
         power x 0 = 1
         power x n = n * (power (n-1))

       The above function is NOT tail-recursive.
       Can you write a tail-recursive
       version of this function which would accumulate its
       result in a 3rd paramater, called acc, as shown below?

*)
let power2 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power2 is yet to be implemented"
  in aux x n 1


(* 
  Q10 : 
       We can also get a logarithmic-time function using
         power x 0 = 1
         power x (2*n = power (x^2) n
         power x (2*n+1) = x*(power (x^2) n)
       Implement such a function tail-recursively.
       How does this compare with the cryptic version of the code
       shown in Lecture 1.
*)
let power3 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power3 is yet to be implemented"
  in aux x n 1
