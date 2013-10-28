use "../incl/finite_map.sml" ;;
use "unbalanced_set.sml" ;;

functor UnbalancedMap (Set: SET, KeyT: ORDERED, ValueT) : FiniteMap =
struct
  type Key = KeyT

  structure OrderedPair: ORDERED =
  struct
  	type T = KeyT * ValueT
  	val eq ((k1, _), (k2, _)) = k1 = k2
  	val lt ((k1, _), (k2, _)) = k1 < k2
  	val leq ((k1, _), (k2, _)) = k1 <= k2
  end

  val empty = Set.empty




end
