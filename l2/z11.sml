
datatype tree = & of int * (tree * tree) | %
infix &

fun	cbt 0 _ = %
  | cbt h v =
    let
      val subtree = cbt (h - 1) v
    in
      v&(subtree, subtree)
    end

fun bt 0 _ = %
  | bt n v =
    let
      val n1 = (n - 1) div 2
      val st1 = bt n1 v
      val st2 = bt (n - 1 - n1) v
    in
      v&(st1, st2)
    end

val t0 = cbt 0 10
val t1 = cbt 1 10
val t2 = cbt 2 10
val t3 = cbt 3 10

val bt0 = bt 0 10
val bt1 = bt 1 10
val bt2 = bt 2 10
val bt3 = bt 3 10
val bt4 = bt 4 10
val bt5 = bt 5 10
val bt6 = bt 6 10

