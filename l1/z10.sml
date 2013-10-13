
datatype tree = & of int * (tree * tree) | %
infix &

fun member (x, y&(t1, t2)) =
    if x <= y
      then if y <= x
        then true
        else member (x, t1)
      else member (x, t2)
  | member (x, %) = false

fun insert (x, t as y&(t1, t2)) =
    if x <= y
      then if y <= x
        then t
        else y&(insert (x, t1), t2)
      else y&(t1, insert (x, t2))
  | insert (x, %) = x&(%,%)

val t1 = insert (5, %)
val t2 = insert (2, t1)
val t3 = insert (8, t2)
val t4 = insert (9, t3)

val mem1 = member (8, t4)
val mem2 = member (7, t4)