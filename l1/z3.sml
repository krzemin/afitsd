
fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x :: xs, y :: ys) = 
  	if x < y then x :: merge (xs, y :: ys)
  	else y :: merge (x :: xs, ys)

fun splitAt n lst = (List.take (lst, n), List.drop (lst, n))

fun msort xs =
  let
  	val n = length xs div 2
  in
  	if n = 0 then xs
  	else
  	  let
  	    val (left, right) = splitAt n xs
  	  in
  		merge (msort left, msort right)
  	  end
  end


val m1 = merge ([], [])
val m2 = merge ([1,2], [])
val m3 = merge ([], [1,2])
val m4 = merge ([1,4,6,7,9], [2,3,5,8])

val s1 = splitAt 2 [1,2,3,4]
val s2 = splitAt 3 [1,2,3,4,5,6,7]

val msort1 = msort []
val msort2 = msort [1]
val msort3 = msort [2,1]
val msort4 = msort [5,2,6,1,2,8,3,7,0,9]
