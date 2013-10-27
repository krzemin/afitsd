use "../incl/ordered.sml" ;;

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
  val fromList: Elem.T list -> Heap
end

exception EMPTY

functor LeftistHeap (Element: ORDERED): HEAP = 
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T(r, _, _, _)) = r

  fun makeT (x, a, b) =
  	if rank a >= rank b
  	  then T(rank b + 1, x, a, b)
  	  else T(rank a + 1, x, b, a)

  val empty = E
  fun isEmpty E = true
  	| isEmpty _ = false

  fun merge (h, E) = h
  	| merge (E, h) = h
  	| merge (h1 as T(_, x, a1, b1), h2 as T(_, y, a2, b2)) =
  	  if Elem.leq (x,y)
  	    then makeT(x, a1, merge(b1, h2))
  	    else makeT(y, a2, merge(h1, b2))

  fun insert (x, h) = merge (T(1, x, E, E), h)
  fun findMin E = raise EMPTY
    | findMin (T(_, x, _, _)) = x
  fun deleteMin E = raise EMPTY
  	| deleteMin (T(_, _, a, b)) = merge (a,b)

  fun fromList elems =
    let
      fun mergeList (h1::h2::hs) = merge (h1, h2) :: mergeList hs
        | mergeList heaps = heaps
      fun iter nil = nil
        | iter [h] = [h]
        | iter heaps = iter (mergeList heaps)
      val iterResult = iter (map (fn e => makeT(e,E,E)) elems)
    in
      hd iterResult handle Error => E
    end
end


structure LeftistHeapInt = LeftistHeap(OrderedInt)

val t1 = LeftistHeapInt.fromList nil
val t2 = LeftistHeapInt.fromList [1]
val t3 = LeftistHeapInt.fromList [1,2]
val t4 = LeftistHeapInt.fromList [1,2,3]
val t5 = LeftistHeapInt.fromList [1,2,3,4]
val t6 = LeftistHeapInt.fromList [1,2,3,4,5]
val t7 = LeftistHeapInt.fromList [10,1,9,2,8,3,7,4,5,6]
