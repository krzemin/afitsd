use "../incl/heap.sml" ;;

functor LeftistHeapDirectInsert (Element: ORDERED): HEAP = 
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

  fun insert (x, E) = T(1, x, E, E)
  	| insert (x, (T(r, y, a, b))) = 
  	if Elem.leq (x, y)
  	  then makeT (x, a, insert (y, b))
  	  else makeT (y, a, insert (x, b))

  fun findMin E = raise EMPTY
    | findMin (T(_, x, _, _)) = x
  fun deleteMin E = raise EMPTY
  	| deleteMin (T(_, _, a, b)) = merge (a,b)

end