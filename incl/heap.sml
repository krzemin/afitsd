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
end

exception EMPTY

functor Flattener(Heap: HEAP) =
struct
	fun flat h = if Heap.isEmpty h
	  then []
	  else
	    let
	      val minElem = Heap.findMin h
	    in
	      minElem :: flat (Heap.deleteMin h)
	    end
end
