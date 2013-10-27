use "weight_biased_leftist_heap.sml" ;;

structure WghtBiasedHeapInt = WghtBiasedHeap(OrderedInt)

val t1 = WghtBiasedHeapInt.empty
val t2 = WghtBiasedHeapInt.insert (5, t1)
val t3 = WghtBiasedHeapInt.insert (2, t2)
val t4 = WghtBiasedHeapInt.insert (7, t3)
val t5 = WghtBiasedHeapInt.insert (0, t4)
val t6 = WghtBiasedHeapInt.insert (3, t5)

