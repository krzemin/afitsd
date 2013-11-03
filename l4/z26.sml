use "../incl/stream.sml" ;;

structure LazyStream : STREAM =
struct

  fun sHd Nil = raise SHd
    | sHd (Cons (hd, _)) = hd



end



