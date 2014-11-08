let mt (x:char) (y:char) = if x=y then 1 else -1
 
let score (xs:string) (ys:string) =
    let x_len = String.length xs in
    let y_len = String.length ys in
    let _ = print_endline ("xs len :"^(string_of_int x_len)) in
    let _ = print_endline ("ys len :"^(string_of_int y_len)) in
 
    let rec aux (xi:int) (yi:int) =
      if xi=x_len && yi==y_len then 0
      else if (yi==y_len) then (x_len-xi)*(-2)
      else if (xi==x_len) then (y_len-yi)*(-2)
      else
        let r1 = -2+aux xi (yi+1) in
        let r2 = -2+aux (xi+1) (yi) in
        let m = mt (String.get xs xi) (String.get ys yi) in
        let r3 = m+aux (xi+1) (yi+1) in
        max r1 (max r2 r3)
 
  in aux 0 0;;
 
let ans1 = score "A" "A";;
let ans2 = score "A" "AB";;
let ans3 = score "GATCGGCAT" "CAATGTGAATC";;
 
print_endline(string_of_int ans1);;
print_endline(string_of_int ans2);;
print_endline(string_of_int ans3);;