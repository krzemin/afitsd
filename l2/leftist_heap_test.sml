use "leftist_heap.sml" ;;

structure LeftistHeapInt = LeftistHeap(OrderedInt)

val t1 = LeftistHeapInt.empty
val t2 = LeftistHeapInt.insert (5, t1)
val t3 = LeftistHeapInt.insert (2, t2)
val t4 = LeftistHeapInt.insert (7, t3)
val t5 = LeftistHeapInt.insert (0, t4)
