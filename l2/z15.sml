use "leftist_heap.sml" ;;


functor WghtBiasedHeap (Element: ORDERED): HEAP =
struct
  structure Elem = Element
  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T(r, _, _, _)) = r

  fun makeT (x, a, b) =
  	if rank a >= rank b
  	  then T(1 + (rank a) + (rank b), x, a, b)
  	  else T(1 + (rank a) + (rank b), x, b, a)

  val empty = E
  fun isEmpty E = true
    | isEmpty _ = false

  fun merge (h, E) = h
  	| merge (E, h) = h
  	| merge (h1 as T(_, x, a1, b1), h2 as T(_, y, a2, b2)) =
  	  if Elem.leq (x,y)
  	    then 
  	      if rank a1 >= rank b1 + rank h2
  	        then T(1 + rank a1 + rank b1 + rank h2, x, a1, merge(b1, h2))
  	        else T(1 + rank a1 + rank b1 + rank h2, x, merge(b1, h2), a1)
  	    	(*makeT(x, a1, merge(b1, h2))*)
  	    else
  	      if rank a2 >= rank h1 + rank b2
  	        then T(1 + rank a2 + rank h1 + rank b2, y, a2, merge(h1, b2))
  	        else T(1 + rank a2 + rank h1 + rank b2, y, merge(h1, b2), a2)
  	    	(*makeT(y, a2, merge(h1, b2))*)

  fun insert (x, h) = merge (T(1, x, E, E), h)

  fun findMin E = raise EMPTY
    | findMin (T(_, x, _, _)) = x

  fun deleteMin E = raise EMPTY
  	| deleteMin (T(_, _, a, b)) = merge (a, b)
end

structure WghtBiasedHeapInt = WghtBiasedHeap(OrderedInt)

val t1 = WghtBiasedHeapInt.empty
val t2 = WghtBiasedHeapInt.insert (5, t1)
val t3 = WghtBiasedHeapInt.insert (2, t2)
val t4 = WghtBiasedHeapInt.insert (7, t3)
val t5 = WghtBiasedHeapInt.insert (0, t4)
val t6 = WghtBiasedHeapInt.insert (3, t5)

