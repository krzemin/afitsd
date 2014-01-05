use "random_access_list.sml" ;;

structure DenseRList : RandomAccessList = 
struct
	datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
	datatype 'a Digit = Zero | One of 'a Tree
	type 'a RList = 'a Digit list

	val empty = []

	fun isEmpty [] = true
		| isEmpty _ = false

  fun size (Leaf _) = 1
  	| size (Node(w, _, _)) = w

  fun link (t1, t2) = Node(size t1 + size t2, t1, t2)

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

	fun lookupTree (0, Leaf x) = x
		| lookupTree (i, Node (w, t1, t2)) =
			if i < w div 2 then lookupTree (i, t1)
			else lookupTree (i - w div 2, t2)

	fun lookup (i, Zero :: ts) = lookup (i, ts)
		| lookup (i, One t :: ts) = 
			if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

	fun updateTree (0, y, Leaf x) = Leaf y
		| updateTree (i, y, Node(w, t1, t2)) =
			if i < w div 2 then Node(w, updateTree(i, y, t1), t2)
			else Node(w, t1, updateTree(i - w div 2, y, t2))

	fun update (i, y, Zero :: ts) = Zero :: update(i, y, ts)
	  | update (i, y, One t :: ts) =
	  	if i < size t then One (updateTree(i, y, t)) :: ts
	  	else One t :: update(i - size t, y, ts)

	fun flattenTree (Leaf x) = [x]
		| flattenTree (Node(_, t1, t2)) = flattenTree t1 @ flattenTree t2

	fun flatten [] = []
	  | flatten (Zero :: ts) = flatten ts
	  | flatten (One t :: ts) = flattenTree t @ flatten ts


  (* z62 *)
	fun drop (0, ts) = ts
		| drop (k, ts) = ts

end

val ral = DenseRList.empty
val ral2 = DenseRList.cons(2, ral)
val ral52 = DenseRList.cons(5, ral2)
val ral752 = DenseRList.cons(7, ral52)
val ral3752 = DenseRList.cons(3, ral752)
val ral13752 = DenseRList.cons(1, ral3752)
val ral13052 = DenseRList.update(2, 0, ral13752)

val ral13752_head = DenseRList.head ral13752
val ral13752_tail = DenseRList.tail ral13752
val ral13752_2nd = DenseRList.lookup (2, ral13752)
val ral13752_3rd = DenseRList.lookup (3, ral13752)
val ral13752_4th = DenseRList.lookup (4, ral13752)
val ral13052_2nd = DenseRList.lookup (2, ral13052)

val ral13052_lst = DenseRList.flatten ral13052
val ral052 = DenseRList.drop (2, ral13052)

