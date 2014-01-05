use "random_access_list.sml" ;;

structure DenseRList : RandomAccessList = 
struct
	datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
	datatype 'a Digit = Zero | One of 'a Tree
	type 'a RList = 'a Digit list

	val empty = []

	fun isEmpty [] = true
		| isEmpty _ = false

  fun rank (Leaf _) = 0
  	| rank (Node(r, _, _)) = r

  fun link (t1, t2) = Node(1 + rank t1, t1, t2)

	fun consTree (t, []) = [One t]
		| consTree (t, Zero :: ts) = One t :: ts
		| consTree (t1, One t2 :: ts) = Zero :: consTree(link(t1, t2), ts)

	fun cons (x, ts) = consTree (Leaf x, ts)




end