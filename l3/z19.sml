use "explicit_min.sml" ;;

use "../l2/leftist_heap.sml" ;;
use "../l2/binomial_heap.sml" ;;
use "../l2/leftist_heap_direct_insert.sml" ;;
use "../l2/weight_biased_leftist_heap.sml" ;;

(*structure HeapUnderTest = LeftistHeap(OrderedInt)*)
(*structure HeapUnderTest = ExplicitMin(LeftistHeap(OrderedInt))*)
(*structure HeapUnderTest = LeftistHeap(OrderedInt)*)
(*structure HeapUnderTest = ExplicitMin(LeftistHeap(OrderedInt))*)
(*structure HeapUnderTest = LeftistHeapDirectInsert(OrderedInt)*)
(*structure HeapUnderTest = ExplicitMin(LeftistHeapDirectInsert(OrderedInt))*)
(*structure HeapUnderTest = WghtBiasedHeap(OrderedInt)*)
(*structure HeapUnderTest = ExplicitMin(WghtBiasedHeap(OrderedInt))*)


val rnd = Random.rand (13, 63)

fun randomIntList 0 = []
  | randomIntList n = Random.randNat rnd :: randomIntList (n - 1)

fun buildHeap lst = foldl (fn (e, h) => HeapUnderTest.insert (e, h)) HeapUnderTest.empty lst
fun flatHeap h =
  if HeapUnderTest.isEmpty h then []
  else HeapUnderTest.findMin h :: flatHeap (HeapUnderTest.deleteMin h)

val r1 = randomIntList 1000

val s1 = flatHeap (buildHeap r1)

