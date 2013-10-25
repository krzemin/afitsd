
signature ORDERED =
sig
  type T
  val leq: T * T -> bool
end

signature HEAP =
sig
  structure Elem: ORDERED

  type Heap

  val empty: Heap
  val isEmpty: Heap -> bool  
  val insert: Elem.T * Heap -> Heap
  val merge: Heap * Heap -> Heap
  val findMin: Heap -> Elem.T
  val deleteMin: Heap -> Heap
  val flat: Heap -> Elem.T list
end

exception EMPTY


functor BinomialHeap(Element: ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, c)) = r
  fun root (Node (r, x, c)) = x
  fun link (t1 as Node(r, x1, c1), t2 as Node(_, x2, c2)) =
  	if Elem.leq (x1, x2) then Node (r + 1, x1, t2 :: c1)
  	else Node(r + 1, x2, t1 :: c2)

  fun insTree (t, []) = [t]
  	| insTree (t, ts as t' :: ts') =
  	  if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

  fun insert (x, ts) = insTree (Node (0, x, []), ts)
  fun merge (ts1, []) = ts1
  	| merge ([], ts2) = ts2
  	| merge (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
  	  if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
  	  else if rank t2 < rank t1 then t2 :: merge (ts1, ts2')
  	  else insTree (link(t1, t2), merge (ts1', ts2'))

  fun removeMinTree [] = raise EMPTY
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
      let
      	val (t', ts') = removeMinTree ts
      in
      	if Elem.leq (root t, root t') then (t, ts) else (t', t::ts')
      end

  fun findMin ts =
  	let
  	  val (t, _) = removeMinTree ts
  	in
  	  root t
  	end

  fun deleteMin ts =
  	let
  	  val (Node (_, x, ts1), ts2) = removeMinTree ts
  	in
  	  merge (rev ts1, ts2)
  	end

  fun flat [] = []
    | flat ts =
      let
        val minElem = findMin ts
      in
        minElem :: flat (deleteMin ts)
      end

end



structure OrderedInt: ORDERED = 
struct
  type T = int
  val leq = (op <=)
end

structure BinomialHeapInt = BinomialHeap(OrderedInt)


val t1 = BinomialHeapInt.empty
val t2 = BinomialHeapInt.insert (5, t1)
val t3 = BinomialHeapInt.insert (2, t2)
val t4 = BinomialHeapInt.insert (7, t3)
val t5 = BinomialHeapInt.insert (0, t4)

val f1 = BinomialHeapInt.flat t1
val f2 = BinomialHeapInt.flat t2
val f3 = BinomialHeapInt.flat t3
val f4 = BinomialHeapInt.flat t4
val f5 = BinomialHeapInt.flat t5

