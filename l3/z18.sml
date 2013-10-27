
use "../l2/binomial_heap.sml" ;;

functor ExplicitMin(H : HEAP) : HEAP =
struct
  structure Elem = H.Elem
  datatype Heap = E | NE of Elem.T * H.Heap

  val empty = E

  fun isEmpty E = true
  	| isEmpty _ = false
  
  fun insert (e, E) = NE (e, H.insert (e, H.empty))
  	| insert (e, NE (e', h)) = if Elem.leq(e, e')
  		then NE(e, H.insert (e, h))
  		else NE(e', H.insert (e, h))

  fun merge (E, E) = E
  	| merge (E, h) = h
  	| merge (h, E) = h
  	| merge (NE (e1, h1), NE (e2, h2)) = if Elem.leq(e1, e2)
  		then NE (e1, H.merge (h1, h2))
  		else NE (e2, H.merge (h1, h2))

  fun findMin E = raise EMPTY
  	| findMin (NE (minElem, _)) = minElem

  fun deleteMin E = raise EMPTY
    | deleteMin (NE (_, h)) =
    let
      val hDeleted = H.deleteMin h
    in
      if H.isEmpty hDeleted
      	then E
      	else NE (H.findMin hDeleted, hDeleted)
	end

  fun flat E = []
  	| flat (NE (_, h)) = H.flat h

end

structure BinHeapExplMinInt = ExplicitMin(BinomialHeapInt)

val t1 = BinHeapExplMinInt.empty
val t2 = BinHeapExplMinInt.insert (5, t1)
val t3 = BinHeapExplMinInt.insert (2, t2)
val t4 = BinHeapExplMinInt.insert (7, t3)
val t5 = BinHeapExplMinInt.insert (0, t4)

val f1 = BinHeapExplMinInt.flat t1
val f2 = BinHeapExplMinInt.flat t2
val f3 = BinHeapExplMinInt.flat t3
val f4 = BinHeapExplMinInt.flat t4
val f5 = BinHeapExplMinInt.flat t5
