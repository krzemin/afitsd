

fun sublist lst =
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
  end


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

