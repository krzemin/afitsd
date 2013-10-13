
fun partition _ [] (acc1, acc2) = (acc1, acc2)
  | partition p (x :: xs) (acc1, acc2) =
  	if x < p then partition p xs (x :: acc1, acc2)
  	else partition p xs (acc1, x :: acc2)

val p1 = partition 5 [1,2,3,4,5,6,7,8,9] ([], [])
val p2 = partition 2 [] ([], [])

fun qsortacc [] acc = acc
  | qsortacc (x :: xs) acc =
  let
  	val (left, right) = partition x xs ([], [])
  in
  	qsortacc left (x :: (qsortacc right acc))
  end

fun qsort lst = qsortacc lst []

val q1 = qsort []
val q2 = qsort [1]
val q3 = qsort [6,3,7,9,2,1]
val q4 = qsort [1,1,2,5,5,5,6,7,7,8,8,8]
val q5 = qsort [9,9,8,7,6,5,4,3,2,2,1,1]
