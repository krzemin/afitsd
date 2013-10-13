

fun sublist (x :: xs) =
  let 
    fun addx (ys :: yss) = (x :: ys) :: addx yss
      | addx nil = nil
    val xss = sublist xs
  in
    xss @ addx xss
  end
  | sublist [] = [[]]

datatype tree = & of int * (tree * tree) | %
infix &
fun flatten (x&(t1,t2)) = flatten t1 @ [x] @ flatten t2
  | flatten % = nil

fun rev (x :: xs) = rev xs @ [x]
  | rev nil = nil

val s1 = sublist [1,2,3]

val f1 = flatten %
val f2 = flatten (5&(%,%))
val f3 = flatten (5&(10&(%,%),15&(%,%)))
val f4 = flatten (1&(2&(%,4&(%,6&(%,%))),3&(5&(%,%),%)))

val r1 = rev [0,1,2,3,4,5,6,7,8,9]
