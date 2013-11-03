use "../incl/stream.sml" ;;

structure LazyStream : STREAM =
struct
  datatype 'a streamCell = Cons of 'a * 'a stream | Nil
  withtype 'a stream = 'a streamCell susp
  exception SHd and STl and Nth

  fun sHd s = case force s of
  	Nil => raise SHd |
  	Cons(hd, _) => hd
  
  fun sTl s = case force s of
  	Nil => raise STl |
  	Cons(_, tl) => tl

  infix ++
  fun op ++(s1, s2) = case force s1 of
  	Nil => s2 |
  	Cons(hd, tl) => delay (fn () => Cons(hd, tl ++ s2))

  

end

