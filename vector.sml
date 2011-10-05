(*
	Vector
	Polymorphic Vectors
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
signature VECTOR = sig
	type 'a t = 'a vector
	include CREATEABLE_SEQUENCE where type 'a sequence = 'a t
	include PURE_SEQUENCE where type 'a pureSequence = 'a t

	type 'a vector = type 'a t
end

structure Vector :> VECTOR = struct
	open Vector
	type 'a t = 'a vector
	type 'a sequence = 'a t
	type 'a pureSequence = 'a t
	fun isEmpty t = length t = 0
	fun hd t = sub (t, 0)
	val appl = app
	val appli = appi
	fun appr f t = foldr (fn (x, ()) => f x) () t
	fun appri f t = foldri (fn (i, x, ()) => f (i, x)) () t
	val fold = foldl
	fun reducel f t =
		let
			val n = length t
			fun loop (i, a) =
				if i >= n then a
				else loop (i + 1, f (sub (t, i), a))
		in
			if n = 0 then raise Empty
			else loop (1, sub (t, 0))
		end
	val reduce = reducel
	fun reducer f t =
		let
			fun loop (i, a) =
				if i < 0 then a
				else loop (i - 1, sub (t, i), a)
			val n = length t
		in
			if n = 0 then raise Empty
			else loop (n - 2, sub (t, n -1))
		end
	val findl = find
	val findli = findi
	fun findr f t =
		let
			fun loop i =
				if i < 0 then NONE
				else
					let
						val x = sub (t, i)
					in
						if f x then SOME x
						else loop (i - 1)
					end
		in
			loop (length t - 1)
		end
	fun findri f t =
		let
			fun loop i =
				if i < 0 then NONE
				else
					let
						val x = (i, sub (t, i))
					in
						if f x then SOME x
						else loop (i - 1)
					end
		in
			loop (length t - 1)
		end
	val toShadow = Shadow.fromVector
	val revToShadow = Shadow.fromRevVector
	fun toList t = foldr (fn (x, y) => x :: y) nil t
	fun revToList t = foldl (fn (x, y) => x :: y) nil t
	fun toTabulated tabulate t = tabulate (length t, fn i => sub (t, i))
	fun revToTabulated tabulate t =
		let
			val n = length t
		in
			tabulate (n, fn i => sub (t, n - 1 - i))
		end
	fun toArray t = toTabulated Array.tabulate t
	fun revToArray t = revToTabulated Array.tabulate t
	val toCharArray = toTabulated CharArray.tabulate
	val revToCharArray = revToTabulated CharArray.tabulate
	val toBoolArray = toTabulated BoolArray.tabulate
	val revToBoolArray = revToTabulated BoolArray.tabulate
	val toIntArray = toTabulated IntArray.tabulate
	val revToIntArray = revToTabulated IntArray.tabulate
	val toWordArray = toTabulated WordArray.tabulate
	val revToWordArray = revToTabulated WordArray.tabulate
	val toRealArray = toTabulated RealArray.tabulate
	val revToRealArray = revToTabulated RealArray.tabulate
	val toFixedIntArray = toTabulated FixedIntArray.tabulate
	val revToFixedIntArray = revToTabulated FixedIntArray.tabulate
	val toLargeIntArray = toTabulated LargeIntArray.tabulate
	val revToLargeIntArray = revToTabulated LargeIntArray.tabulate
	fun unfold f (n, a) =
		let
			val (vector, _) = MLton.Vector.unfoldi (n, a, fn (_, a) => f a)
		in
			vector
		end
	fun unfoldi f (n, a) =
		let
			val (vector, _) = MLton.Vector.unfoldi (n, a, f)
		in
			vector
		end
end
