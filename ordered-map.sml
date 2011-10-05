(*
	Ordered Maps
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

signature ORDERED_MAP = sig
	structure Key: ORDERED
	exception NotFound
	type 'a t
	val isEmpty: 'a t -> bool
	val length: 'a t -> int
	val empty: 'a t
	val single: Key.t * 'a -> 'a t
	val put: 'a t * Key.t * 'a -> 'a t
	val remove: 'a t * Key.t -> 'a t
	val get: 'a t * Key.t -> 'a option
	val descend: 'a t * Key.t -> (Key.t * 'a) Sequence.t
	val member: 'a t * Key.t -> bool
	val getFirst: 'a t -> ((Key.t * 'a) * 'a t) option
	val first: 'a t -> Key.t * 'a
	val getLast: 'a t -> ((Key.t * 'a) * 'a t) option
	val last: 'a t -> Key.t * 'a
	(*
	val less: 'a t * Key.t -> 'a Sequence.t
	val greater: 'a t * Key.t -> 'a Sequence.t
	val lessi: 'a t * Key.t -> (Key.t * 'a) Sequence.t
	val greateri: 'a t * Key.t -> (Key.t * 'a) Sequence.t
	val fromList: (Key.t * 'a) list -> 'a t
	val fromVector: (Key.t * 'a) vector -> 'a t
	val fromSortedVector: (Key.t * 'a) vector -> 'a t
	val fromArray: (Key.t * 'a) array -> 'a t
	val fromSortedArray: (Key.t * 'a) array -> 'a t
	val fromSequence: (Key.t * 'a) Sequence.t -> 'a t
	val toListl: 'a t -> (Key.t * 'a) list
	val toListr: 'a t -> (Key.t * 'a) list
	val toVectorl: 'a t -> (Key.t * 'a) vector
	val toVectorr: 'a t -> (Key.t * 'a) vector
	val toArrayl: 'a t -> (Key.t * 'a) array
	val toArrayr: 'a t -> (Key.t * 'a) array
	val toSequencel: 'a t -> (Key.t * 'a) Sequence.t
	val toSequencer: 'a t -> (Key.t * 'a) Sequence.t
	val collate: ('a * 'a -> order) -> 'a t * 'a t -> order
	val appl: ('a -> unit) -> 'a t -> unit
	val appr: ('a -> unit) -> 'a t -> unit
	val appli: (Key.t * 'a -> unit) -> 'a t -> unit
	val appri: (Key.t * 'a -> unit) -> 'a t -> unit
	val map: ('a -> 'b) -> 'a t -> 'b t
	val mapi: (Key.t * 'a -> 'b) -> 'a t -> 'b t
	val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
	val mapPartiali: (Key.t * 'a -> 'b option) -> 'a t -> 'b t
	val foldl: ('a * 'b -> 'b) -> 'b -> 'a -> 'b
	val foldr: ('a * 'b -> 'b) -> 'b -> 'a -> 'b
	val foldli: (Key.t * 'a * 'b -> 'b) -> 'b -> 'a -> 'b
	val foldri: (Key.t * 'a * 'b -> 'b) -> 'b -> 'a -> 'b
	val exists: ('a -> bool) -> 'a t -> bool
	val all: ('a -> bool) -> 'a t -> bool
	val existsi: (Key.t * 'a -> bool) -> 'a t -> bool
	val alli: (Key.t * 'a -> bool) -> 'a t -> bool
	val union: ('a * 'a -> 'a) -> 'a t * 'a t -> 'a t
	val unioni: (Key.t * 'a * 'a -> 'a) -> 'a t * 'a t -> 'a t
	val intersect: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
	val intersecti: (Key.t * 'a * 'b -> 'c) -> 'a t * 'b t -> 'c t
	val difference: 'a t * 'b t -> 'a t
	val merge: ('a option * 'b option -> 'c option) -> 'a t * 'b t -> 'c t
	val mergei: (Key.t * 'a option * 'b option -> 'c option) -> 'a t * 'b t -> 'c t
	*)
end

