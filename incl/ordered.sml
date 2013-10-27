signature ORDERED =
sig
  type T
  val leq: T * T -> bool
end

structure OrderedInt: ORDERED = 
struct
  type T = int
  val leq = (op <=)
end
