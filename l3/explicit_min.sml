use "../incl/heap.sml" ;;


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
    | merge (NE (e1, h1), NE (e2, h2)) =
    if Elem.leq(e1, e2)
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

end
