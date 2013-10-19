

(*fun sublist lst =
  let
    fun sublistacc nil acc = acc
      | sublistacc (x :: xs) acc =
      let
        val expanded = map (fn ys => [ys, x :: ys]) acc
      in
        sublistacc xs (List.concat expanded)
      end
  in
    sublistacc lst [[]]
  end*)

fun sublist xs =
  foldr (fn (x, xss) => foldl (fn (ys, zss) => (x::ys)::zss) xss xss) [[]] xs


datatype tree = & of int * (tree * tree) | %
infix &
fun flatten t =
  let
    fun flattenacc % acc = acc
      | flattenacc (x&(t1,t2)) acc = flattenacc t1 (x :: (flattenacc t2 acc))
  in
    flattenacc t nil
  end


fun rev lst =
  let
    fun revacc nil acc = acc
      | revacc (x :: xs) acc = revacc xs (x :: acc)
  in
    revacc lst nil
  end

val s1 = sublist [1,2,3]

val f1 = flatten %
val f2 = flatten (5&(%,%))
val f3 = flatten (5&(10&(%,%),15&(%,%)))
val f4 = flatten (1&(2&(%,4&(%,6&(%,%))),3&(5&(%,%),%)))

val r1 = rev [0,1,2,3,4,5,6,7,8,9]
