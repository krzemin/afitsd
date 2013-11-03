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

  fun constStream a = delay (fn () => Cons(a, constStream a))

  fun mkStream f =
  	let
  		fun aux n = delay (fn () => Cons(f n, aux (n + 1)))
  	in
  		aux 0
  	end

  fun from n = delay (fn () => Cons(n, from (n + 1)))

  fun sMap f s = case force s of
  	Nil => delay (fn() => Nil) |
  	Cons(hd, tl) => delay (fn () => Cons(f hd, sMap f tl))

  fun sDrop 0 s = s
  	| sDrop n s = case force s of
  	  Nil => delay (fn() => Nil) |
  	  Cons(_, tl) => sDrop (n - 1) tl

  fun sTake 0 _ = delay (fn() => Nil)
  	| sTake n s = case force s of
  	  Nil => delay (fn() => Nil) |
  	  Cons(hd, tl) => delay (fn () => Cons(hd, sTake (n - 1) tl))

  fun zip (s1, s2) = case force s1 of
  	Nil => delay (fn() => Nil) |
  	Cons(hd1, tl1) => case force s2 of
  		Nil => delay (fn() => Nil) |
  		Cons(hd2, tl2) => delay (fn () => Cons((hd1,hd2), zip(tl1, tl2)))

(*
  monolithic version
  fun unzip s = case force s of
  	Nil => delay (fn() => Nil) |
  	Cons((hd1, hd2), tl) =>
  		let val (tl1, tl2) = unzip tl
  		in (delay (fn () => Cons(hd1, tl1)), delay (fn () => Cons(hd2, tl2))) end
*)
  (*incremental version*)
  fun unzip s =
  	let
      fun leftPart s = case force s of
        Nil => delay (fn() => Nil) |
        Cons((hd1, _), tl) => delay (fn() => Cons(hd1, leftPart tl))
      fun rightPart s = case force s of
        Nil => delay (fn() => Nil) |
        Cons((_, hd2), tl) => delay (fn() => Cons(hd2, rightPart tl))
    in
      (leftPart s, rightPart s)
    end

end

val s1 = LazyStream.constStream 1
val s2 = LazyStream.constStream 2
val s12 = LazyStream.zip (s1, s2)
val (u1, u2) = LazyStream.unzip s12

