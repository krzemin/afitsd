
use "leftist_heap_direct_insert.sml" ;;

structure LeftistHeapDirectInsertInt = LeftistHeapDirectInsert(OrderedInt)

val t1 = LeftistHeapDirectInsertInt.empty
val t2 = LeftistHeapDirectInsertInt.insert (5, t1)
val t3 = LeftistHeapDirectInsertInt.insert (2, t2)
val t4 = LeftistHeapDirectInsertInt.insert (7, t3)
val t5 = LeftistHeapDirectInsertInt.insert (0, t4)

