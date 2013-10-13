

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


