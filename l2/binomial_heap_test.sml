
use "binomial_heap.sml" ;;

structure BinomialHeapInt = BinomialHeap(OrderedInt)

val t1 = BinomialHeapInt.empty
val t2 = BinomialHeapInt.insert (5, t1)
val t3 = BinomialHeapInt.insert (2, t2)
val t4 = BinomialHeapInt.insert (7, t3)
val t5 = BinomialHeapInt.insert (0, t4)

structure F = Flattener(BinomialHeapInt)

val f1 = F.flat t1
val f2 = F.flat t2
val f3 = F.flat t3
val f4 = F.flat t4
val f5 = F.flat t5
