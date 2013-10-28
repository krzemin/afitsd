signature ORDERED =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

structure OrderedInt: ORDERED = 
struct
  type T = int
  val eq = (op =)
  val lt = (op <)
  val leq = (op <=)
end