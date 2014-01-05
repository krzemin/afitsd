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

	fun unconsTree [One t] = (t, [])
		| unconsTree (One t :: ts) = (t, Zero :: ts)
		| unconsTree (Zero :: ts) =
			let val (Node(_, t1, t2), ts') = unconsTree ts
			in (t1, One t2 :: ts') end

	fun head ts = let val (Leaf x, _) = unconsTree ts in x end

	fun tail ts = let val (_, ts') = unconsTree ts in ts' end





end

val ral1 = DenseRList.empty
val ral2 = DenseRList.cons(5, ral1)
val ral3 = DenseRList.cons(4, ral2)
val ral4 = DenseRList.cons(3, ral3)
val ral5 = DenseRList.cons(2, ral4)
val ral6 = DenseRList.cons(1, ral5)
val ral6_head = DenseRList.head ral6
val ral6_tail = DenseRList.tail ral6



