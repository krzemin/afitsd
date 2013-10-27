
use "../l2/binomial_heap.sml" ;;
use "explicit_min.sml" ;;

structure BinHeapExplMinInt = ExplicitMin(BinomialHeap(OrderedInt))

val t1 = BinHeapExplMinInt.empty
val t2 = BinHeapExplMinInt.insert (5, t1)
val t3 = BinHeapExplMinInt.insert (2, t2)
val t4 = BinHeapExplMinInt.insert (7, t3)
val t5 = BinHeapExplMinInt.insert (0, t4)

structure F = Flattener(BinHeapExplMinInt)

val f1 = F.flat t1
val f2 = F.flat t2
val f3 = F.flat t3
val f4 = F.flat t4
val f5 = F.flat t5
