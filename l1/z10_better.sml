
datatype tree = & of int * (tree * tree) | %
infix &

fun member (elem, tree) =
  let
    fun member_aux (x, y&(t1, t2)) v =
        if x < y
          then member_aux (x, t1) v
          else member_aux (x, t2) (SOME y)
      | member_aux (x, %) v =
      	case v of
      	  NONE => false
      	| SOME y => x = y
  in
    member_aux (elem, tree) NONE
  end

exception ElementExists

fun insert_ex (elem, tree) =
  let
    fun insert_aux (x, y&(t1, t2)) =
      if x <= y
        then if y <= x
          then raise ElementExists
          else y&(insert_aux (x, t1), t2)
        else y&(t1, insert_aux (x, t2))
      | insert_aux (x, %) = x&(%,%)
  in
  	insert_aux (elem, tree)
  	handle ElementExists => tree
  end

fun insert (elem, tree) =
  let
    fun insert_aux (x, y&(t1, t2)) v =
      if x < y
        then y&(insert_aux (x, t1) v, t2)
        else y&(t1, insert_aux (x, t2) (SOME y))
      | insert_aux (x, %) v =
        case v of
          NONE => x&(%, %)
        | SOME y => if x = y
        	then raise ElementExists
        	else x&(%, %)
  in
  	insert_aux (elem, tree) NONE
  	handle ElementExists => tree
  end


val t1 = insert_ex (5, %)
val t2 = insert_ex (2, t1)
val t3 = insert_ex (8, t2)
val t4 = insert_ex (9, t3)

val mem1 = member (8, t4)
val mem2 = member (7, t4)
val mem3 = member (2, t4)
val mem4 = member (9, t4)

val tt1 = insert (5, %)
val tt2 = insert (2, t1)
val tt3 = insert (8, t2)
val tt4 = insert (9, t3)

val tmem1 = member (8, t4)
val tmem2 = member (7, t4)
val tmem3 = member (2, t4)
val tmem4 = member (9, t4)
