signature RandomAccessList = 
sig
	type 'a RList

	val empty : 'a RList
	val isEmpty : 'a RList -> bool
	val cons : 'a * 'a RList -> 'a RList
	val head : 'a RList -> 'a
	val tail : 'a RList -> 'a RList
	val lookup : int * 'a RList -> 'a
(*	val update : int * 'a * 'a RList -> 'a RList

	exception Empty
	exception Subscript*)
end