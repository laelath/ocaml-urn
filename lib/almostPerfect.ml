(* Implements the creation of "almost perfect" binary trees *)

(* best I can do without count leading zeros *)
let log2 x =
  if x <= 0
  then invalid_arg "log2: argument <= 0";
  let rec lp acc x =
    match x with
    | 0 -> assert false
    | 1 -> acc
    | 2 | 3 -> 1 + acc
    | 4 | 5 | 6 | 7 -> 2 + acc
    | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 -> 3 + acc
    | _ -> lp (4 + acc) (x lsr 4)
    in
  lp 0 x

(* TODO: Antal says this can be more efficient
         (but didn't implement a more efficient version) *)
let reverse_bits =
  let rec go r n x =
    match n with
    | 0 -> r
    | _ -> go (Int.logor (Int.shift_left r 1) (Int.logand x 1))
              (Int.pred n)
              (Int.shift_right x 1) in
  go 0

let almost_perfect node leaf size elems0 =
  let perfect_depth = log2 size in
  let remainder = size - Int.shift_left 1 perfect_depth in
  let raise_size_error () =
    invalid_arg
      ("almost_perfect: size mismatch: got input of length " ^
       Int.to_string (List.length elems0) ^
       ", but expected size " ^ Int.to_string size) in
  let rec go depth index elems =
    match depth with
    | 0 ->
       if reverse_bits perfect_depth index < remainder
       then (match elems with
             | l :: r :: elems' ->
                (node (leaf l) (leaf r), elems', Int.succ index)
             | _ -> raise_size_error ())
       else (match elems with
             | x :: elems' ->
                (leaf x, elems', Int.succ index)
             | _ -> raise_size_error ())
    | _ ->
      let (l, elems',  index' ) = go (Int.pred depth) index  elems  in
      let (r, elems'', index'') = go (Int.pred depth) index' elems' in
      (node l r, elems'', index'') in
  let (tree, _, _) = go perfect_depth 0 elems0 in
  tree

