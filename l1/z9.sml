
fun suffixes [] = [[]]
  | suffixes (lst as (_ :: xs)) = lst :: (suffixes xs)

val s1 = suffixes []
val s2 = suffixes [1]
val s3 = suffixes [1,2]
val s4 = suffixes [1,2,3,4]
