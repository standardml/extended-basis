(*
	Leroy Trees
	Copyright 2011 Christopher Cramer

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program. If not, see http://www.gnu.org/licenses/.
*)

(*
	Normally, an AVL tree has the restriction that the two children
	of a node can have a height imbalance of at most 1. If you
	loosen this restriction so that the height imbalance can be 2,
	then you outperform Red-Black trees. To the best of my knowledge,
	Xavier Leroy was the the first to try this, and JH Woodyatt was
	the first to discover the performance relative to Red-Black trees:

	http://caml.inria.fr/pub/ml-archives/caml-list/2006/08/d1ec8eab49ebe7ca40266e6edee0a88c.en.html
	http://caml.inria.fr/pub/ml-archives/caml-list/2006/08/f42e38c628bd4f09ff4eea5cdb9b60b1.en.html

	I have tested height imbalance limits of 1 through 5, and in fact
	2 usually performs best (and it's the only one that usually
	defeats Red-Black trees).
*)

functor Tree (Key: ORDERED) :> ORDERED_MAP where type Key.t = Key.t = struct
	structure Key = Key
	exception NotFound
	exception Exists
	datatype 'a t =
		Null
		| Node of {
			height: int
			, key: Key.t
			, datum: 'a
			, less: 'a t
			, greater: 'a t
		}
	fun height t = case t of
		Null => 0
		| Node {height, ...} => height
	val empty = Null
	fun isEmpty t = case t of Null => true | _ => false
	fun length t = case t of
		Null => 0
		| Node {less, greater, ...} => 1 + length less + length greater
	fun single (key, datum) = Node {
		height = 1
		, key = key
		, datum = datum
		, less = Null
		, greater = Null
	}
	(*
	           x              y
		a     y   =>   x     c
		    b   c    a   b
	*)
	fun rotateLess {key = xKey, datum = xDatum, less = a, greater} = case greater of
		Node {
			key = yKey
			, datum = yDatum
			, less = b
			, greater = c
			, ...
		} =>
			let
				val xHeight = Int.max (height a, height b) + 1
				val yHeight = Int.max (xHeight, height c) + 1
			in
				Node {
					height = Int.max (xHeight, height c)
					, key = yKey
					, datum = yDatum
					, less = Node {
						height = xHeight
						, key = xKey
						, datum = xDatum
						, less = a
						, greater = b
					}, greater = c
				}
			end
		| _ => raise Fail "impossible"
	(*
	             y          x
		  x     c => a     y
		a   b            b   c
	*)
	fun rotateGreater {key = yKey, datum = yDatum, greater = c, less} = case less of
		Node {
			key = xKey
			, datum = xDatum
			, less = a
			, greater = b
			, ...
		} =>
			let
				val yHeight = Int.max (height b, height c) + 1
				val xHeight = Int.max (height a, yHeight) + 1
			in
				Node {
					height = xHeight
					, key = xKey
					, datum = xDatum
					, less = a
					, greater = Node {
						height = yHeight
						, key = yKey
						, datum = yDatum
						, less = b
						, greater = c
					}
				}
			end
		| _ => raise Fail "impossible"
	fun balance (t as {key, datum, greater, less}) =
		let
			val lessHeight = height less
			val greaterHeight = height greater
		in
			if lessHeight > greaterHeight + 2 then
				rotateGreater t
			else if greaterHeight > lessHeight + 2 then
				rotateLess t
			else Node {
				height = Int.max (greaterHeight, lessHeight) + 1
				, key = key
				, datum = datum
				, less = less
				, greater = greater
			}
		end
	fun get (t, wanted) = case t of
		Null => NONE
		| Node {key, datum, less, greater, ...} => (
			case Key.compare (wanted, key) of
				LESS => get (less, wanted)
				| GREATER => get (greater, wanted)
				| EQUAL => SOME datum
		)
	fun put (t, newKey, newDatum) = case t of
		Null => single (newKey, newDatum)
		| Node {key, datum, less, greater, height} => (
			case Key.compare (newKey, key) of
				LESS => balance {
					key = key
					, datum = datum
					, greater = greater
					, less = put (less, newKey, newDatum)
				} | GREATER => balance {
					key = key
					, datum = datum
					, greater = put (greater, newKey, newDatum)
					, less = less
				} | EQUAL => Node {
					height = height
					, key = newKey
					, datum = newDatum
					, less = less
					, greater = greater
				}
		)
	fun getFirst t = case t of
		Null => NONE
		| Node {key, datum, less = Null, greater, ...} =>
			SOME ((key, datum), greater)
		| Node {key, datum, less, greater, ...} => (case getFirst less of
			NONE => SOME ((key, datum), greater)
			| SOME (first, less) => SOME (
				first
				, balance {
					key = key
					, datum = datum
					, less = less
					, greater = greater
				}
			)
		)
	fun getLast t = case t of
		Null => NONE
		| Node {key, datum, less, greater, ...} => (case getLast greater of
			NONE => SOME ((key, datum), less)
			| SOME (last, greater) => SOME (
				last
				, balance {
					key = key
					, datum = datum
					, less = less
					, greater = greater
				}
			)
		)
	fun spliceLess t = case t of
		Null => t
		| Node {less, greater, ...} => (case getLast less of
			NONE => t
			| SOME ((key, datum), less) => balance {
				key = key
				, datum = datum
				, less = less
				, greater = greater
			}
		)
	fun spliceGreater t = case t of
		Null => t
		| Node {less, greater, ...} => (case getFirst greater of
			NONE => t
			| SOME ((key, datum), greater) => balance {
				key = key
				, datum = datum
				, less = less
				, greater = greater
			}
		)
	fun remove (t, unwanted) = case t of
		Null => raise NotFound
		| Node {key, datum, less, greater, ...} => (
			case Key.compare (unwanted, key) of
				LESS => balance {
					key = key
					, datum = datum
					, less = remove (less, unwanted)
					, greater = greater
				} | GREATER => balance {
					key = key
					, datum = datum
					, less = less
					, greater = remove (greater, unwanted)
				} | EQUAL => (case (less, greater) of
					(Null, Null) => Null
					| (Node _, Null) => less
					| (Null, Node _) => greater
					| (Node _, Node _) => 
						if height less < height greater then
							spliceGreater t
						else spliceLess t
				)
		)
	fun member (t, wanted) = case t of
		Null => false
		| Node {key, less, greater, ...} => (
			case Key.compare (wanted, key) of
				LESS => member (less, wanted)
				| GREATER => member (greater, wanted)
				| EQUAL => true
		)
	fun first t = case t of
		Null => raise Empty
		| Node {key, datum, less = Null, ...} => (key, datum)
		| Node {less, ...} => first less
	fun last t = case t of
		Null => raise Empty
		| Node {key, datum, greater = Null, ...} => (key, datum)
		| Node {greater, ...} => last greater
	fun descend (t, start) =
		let
			fun skipGreater x () = case x of
				nil => NONE
				| Null :: y => skipGreater y ()
				| Node {key, datum, less, ...} :: y => SOME (
					(key, datum)
					, Shadow.fromIdempotentFunction (followGreater (less :: y))
				)
			and followGreater x () = case x of
				nil => NONE
				| Null :: y => skipGreater y ()
				| Node {greater = Null, key, datum, less, ...} :: y => SOME (
					(key, datum)
					, Shadow.fromIdempotentFunction (followGreater (less :: y))
				) | Node {greater, ...} :: _ => followGreater (greater :: x) ()
			fun find x () = case x of
				nil => NONE
				| Null :: y => skipGreater y ()
				| Node {key, datum, less, greater, ...} :: y => (
					case Key.compare (start, key) of
						LESS => find (less :: y) ()
						| GREATER => find (greater :: x) ()
						| EQUAL => skipGreater x ()
				)
		in
			Shadow.fromIdempotentFunction (find [t])
		end
	fun ascend (t, start) =
		let
			fun skipLess x () = case x of
				nil => NONE
				| Null :: y => skipLess y ()
				| Node {key, datum, greater, ...} :: y => SOME (
					(key, datum)
					, Shadow.fromIdempotentFunction (followLess (greater :: y))
				)
			and followLess x () = case x of
				nil => NONE
				| Null :: y => skipLess y ()
				| Node {less = Null, key, datum, greater, ...} :: y => SOME (
					(key, datum)
					, Shadow.fromIdempotentFunction (followLess (greater :: y))
				) | Node {less, ...} :: _ => followLess (less :: x) ()
			fun find x () = case x of
				nil => NONE
				| Null :: y => skipLess y ()
				| Node {key, datum, less, greater, ...} :: y => (
					case Key.compare (start, key) of
						GREATER => find (greater :: y) ()
						| LESS => find (less :: x) ()
						| EQUAL => skipLess x ()
				)
		in
			Shadow.fromIdempotentFunction (find [t])
		end
end
