let mt (x: char) (y: char) = if x = y then 1 else -1;;

let score (xs: string) (ys: string) =
  let pts_space = -2 in
  let x_len = String.length xs in
  let y_len = String.length ys in
  let revise
      (acc: int * string * string)
      (pts: int)
      (x_char: char)
      (y_char: char) : int * string * string =
    let (score, xs, ys) = acc in
    (pts + score, String.make 1 x_char ^ xs, String.make 1 y_char ^ ys)
  in let rec aux (xi: int) (yi: int) =
    if xi = x_len && yi == y_len then (0, "", "")
    else if (yi == y_len) then
      let num = x_len - xi in
      let pts = pts_space * num in
      let xs' = String.sub xs xi num in
      let ys' = String.make num ' ' in
      (pts, xs', ys') 
    else if (xi == x_len) then
      let num = y_len - yi in
      let pts = pts_space * num in
      let xs' = String.make num ' ' in
      let ys' = String.sub ys yi num in
      (pts, xs', ys')
    else
      let x_char = String.get xs xi in
      let y_char = String.get ys yi in
      let r1 = revise (aux xi (yi + 1)) pts_space ' ' y_char in
      let r2 = revise (aux (xi + 1) yi) pts_space x_char ' ' in
      let m = mt x_char y_char in
      let r3 = revise (aux (xi + 1) (yi + 1)) m x_char y_char in
      max r1 (max r2 r3)
  in aux 0 0;;

let ans1 = score "A" "A";;
let ans2 = score "A" "AB";;
let ans3 = score "GATCGGCAT" "CAATGTGAATC";;

let pprint (result: int * string * string) : unit =
  let alignment (xs: string) (ys: string) : string =
    let sym (x: char) (y: char) : char =
      if x = y then '+'
      else if x = ' ' || y = ' ' then '*'
      else '-'
    in let len = String.length xs in
    let rec aux (idx: int) (acc: string) =
      if idx = len then acc
      else aux (idx + 1) (acc ^ String.make 1 (sym (String.get xs idx) (String.get ys idx)))
  in aux 0 ""
  in let (score, xs, ys) = result in
  print_endline xs;
  print_endline ys;
  print_endline (alignment xs ys);
  print_endline ("Score: " ^ string_of_int score);;

pprint ans1;;
pprint ans2;;
pprint ans3;;
