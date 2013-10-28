use "../incl/ordered_total.sml" ;;
use "../incl/set.sml" ;;

functor UnbalancedSet (Element: ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree
  
  val empty = E

  fun member (x, E) = false
    | member (x, T (a, y, b)) =
    if Element.lt (x, y) then member (x, a)
    else if Element.lt (y, x) then member (x, b)
    else true

  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (a, y, b)) =
    if Element.lt (x, y) then T (insert (x, a), y, b)
    else if Element.lt (y, x) then T (a, y, insert (x, b))
    else s
end