(*
	MonoArrayOrVector
	Generic Implementations of Monomorphic Random-Access Sequence Functions
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
functor MonoArrayOrVector (T: sig
	type t
	type x
	val length: t -> int
	val sub: t * int -> x
	val tabulate: int * (int -> x) -> t
	val foldl: (x * 'a -> 'a) -> 'a -> t -> 'a
	val foldr: (x * 'a -> 'a) -> 'a -> t -> 'a
	val concat: t list -> t
end) = struct
	fun isEmpty t = T.length t = 0
	fun hd t =
		if isEmpty t then raise Subscript
		else T.sub (t, 0)
	fun appr f t =
		let
			fun loop i =
				if i < 0 then ()
				else (
					f (T.sub (t, i))
					; loop (i - 1)
				)
		in
			loop (T.length t - 1)
		end
	fun appri f t =
		let
			fun loop i =
				if i < 0 then ()
				else (
					f (i, T.sub (t, i))
					; loop (i - 1)
				)
		in
			loop (T.length t - 1)
		end
	fun reducel f t =
		let
			val n = T.length t
			fun loop (i, a) =
				if i >= n then a
				else loop (i + 1, f (T.sub (t, i), a))
		in
			if n = 0 then raise Empty
			else loop (1, T.sub (t, 0))
		end
	fun reducer f t =
		let
			val n = T.length t
			fun loop (i, a) =
				if i < 0 then a
				else loop (i - 1, f (T.sub (t, i), a))
		in
			if n = 0 then raise Empty
			else loop (n - 2, T.sub (t, n - 1))
		end
	fun findr f t =
		let
			fun loop i =
				if i < 0 then NONE
				else
					let
						val x = T.sub (t, i)
					in
						if f x then SOME x
						else loop (i - 1)
					end
		in
			loop (T.length t - 1)
		end
	fun findri f t =
		let
			fun loop i =
				if i < 0 then NONE
				else
					let
						val x = (i, T.sub (t, i))
					in
						if f x then SOME x
						else loop (i - 1)
					end
		in
			loop (T.length t - 1)
		end
	fun toShadow t = Shadow.fromRandom {
		sub = fn i => T.sub (t, i)
		, length = fn () => T.length t
	}
	fun revToShadow t = Shadow.rev (toShadow t)
	fun toList t = T.foldr (fn (x, y) => x :: y) nil t
	fun revToList t = T.foldl (fn (x, y) => x :: y) nil t
	fun toTabulated tabulate t = tabulate (T.length t, fn i => T.sub (t, i))
	fun revToTabulated tabulate t =
		let
			val n = T.length t
		in
			tabulate (n, fn i => T.sub (t, n - 1 - i))
		end
	fun empty () = T.tabulate (0, fn _ => raise Fail "impossible")
	fun single x = T.tabulate (1, fn _ => x)
	fun unfold f a =
		let
			fun construct (n, l) =
				let
					val l = ref l
				in
					T.tabulate (
						n
						, fn _ => (case !l of
							nil => raise Fail "impossible"
							| x :: y => (
								l := y
								; x
							)
						)
					)
				end
			fun loop (n, l, a) = case f a of
				NONE => construct (n, l)
				| SOME (x, a) => loop (n + 1, x :: l, a)
		in
			loop (0, nil, a)
		end
	fun unfoldn f (n, a) =
		let
			val a = ref a
		in
			T.tabulate (
				n
				, fn _ =>
					let
						val (x, b) = f (!a)
					in
						a := b
						; x
					end
			)
		end
	fun splitAt (t, n) = (
		T.tabulate (n, fn i => T.sub (t, i))
		, T.tabulate (T.length t - n, fn i => T.sub (t, n + i))
	)
	fun split f t =
		let
			val n = T.length t
			fun loop i =
				if i >= n then (t, empty ())
				else if f (T.sub (t, i)) then loop (i + 1)
				else splitAt (t, i)
		in
			loop 0
		end
	fun drop f t =
		let
			val n = T.length t
			fun loop i =
				if i >= n then empty ()
				else if f (T.sub (t, i)) then loop (i + 1)
				else T.tabulate (n - i, fn j => T.sub (t, i + j))
		in
			loop 0
		end
	fun extract (t, i, n) = T.tabulate (
		case n of
			NONE => T.length t - i
			| SOME n => n
		, fn j => T.sub (t, i + j)
	)
	fun trim (t, n) = extract (t, n, NONE)
	fun limit (t, n) = extract (t, 0, SOME n)
	fun take f t =
		let
			val n = T.length t
			fun loop i =
				if i >= n then t
				else if f (T.sub (t, i)) then loop (i + 1)
				else limit (t, i)
		in
			loop 0
		end
	fun tokens f t =
		let
			val n = T.length t
			fun token (i, j, l) =
				let
					fun add () = extract (t, i, SOME (j - i)) :: l
				in
					if j >= n then List.rev (add ())
					else if f (T.sub (t, j)) then separator (j + 1, add ())
					else token (i, j + 1, l)
				end
			and separator (i, l) =
				if i >= n then List.rev l
				else if f (T.sub (t, i)) then separator (i + 1, l)
				else token (i, i + 1, l)
		in
			separator (0, nil)
		end
	fun fields f t =
		let
			val n = T.length t
			fun field (i, j, l) =
				let
					fun add () = extract (t, i, SOME (j - i)) :: l
				in
					if j >= n then List.rev (add ())
					else if f (T.sub (t, j)) then field (j + 1, j + 1, add ())
					else field (i, j + 1, l)
				end
		in
			field (0, 0, nil)
		end
	fun translate f t =
		let
			val n = T.length t
			fun loop (i, l) =
				if i >= n then T.concat (List.rev l)
				else loop (i + 1, f (T.sub (t, i)) :: l)
		in
			loop (0, nil)
		end
	fun insert (t, i, x) = T.tabulate (
		T.length t + 1
		, fn j =>
			if j < i then T.sub (t, j)
			else if j = i then x
			else T.sub (t, j - 1)
	)
	fun delete (t, i) = T.tabulate (
		T.length t - 1
		, fn j =>
			if j < i then T.sub (t, j)
			else T.sub (t, j + 1)
	)
	fun fromShadow s = case Shadow.access s of
		Shadow.Random => T.tabulate (Shadow.length s, fn i => Shadow.sub (s, i))
		| Shadow.Sequential => unfoldn
			(fn s => valOf (Shadow.getItem s))
			(Shadow.length s, s)
	fun fromRevShadow s = fromShadow (Shadow.rev s)
	fun fromLengthSub (length, sub) x = T.tabulate (length x, fn i => sub (x, i))
	fun fromRevLengthSub (length, sub) x =
		let
			val n = length x
		in
			T.tabulate (n, fn i => sub (x, n - 1 - i))
		end
	fun getItem t =
		let
			val n = T.length t
		in
			if n = 0 then NONE
			else SOME (
				T.sub (t, 0)
				, T.tabulate (n - 1, fn i => T.sub (t, i + 1))
			)
		end
	fun ungetItem (t, x) = T.tabulate (
		T.length t + 1
		, fn
			0 => x
			| i => T.sub (t, i - 1)
	)
	fun tl t =
		let
			val n = T.length t
		in
			if n = 0 then raise Empty
			else T.tabulate (n - 1, fn i => T.sub (t, i + 1))
		end
	fun append (a, b) =
		let
			val n = T.length a
		in
			T.tabulate (
				n + T.length b
				, fn i =>
					if i < n then T.sub (a, i)
					else T.sub (b, i - n)
			)
		end
	fun filter f t =
		let
			val n = T.length t
			fun return (m, l) = unfoldn (valOf o List.getItem) (m, l)
			fun loop (i, m, l) =
				if i < 0 then return (m, l)
				else
					let
						val x = T.sub (t, i)
					in
						if f x then loop (i - 1, m + 1, x :: l)
						else loop (i - 1, m, l)
					end
		in
			loop (n - 1, 0, nil)
		end
	fun mapPartial f t =
		let
			val n = T.length t
			fun return (m, l) = unfoldn (valOf o List.getItem) (m, l)
			fun loop (i, m, l) =
				if i < 0 then return (m, l)
				else case f (T.sub (t, i)) of
					NONE => loop (i - 1, m, l)
					| SOME x => loop (i - 1, m + 1, x :: l)
		in
			loop (n - 1, 0, nil)
		end
	fun partition f t =
		let
			val n = T.length t
			fun construct (n, l) = unfoldn (valOf o List.getItem) (n, l)
			fun return (yesN, yesL, noN, noL) = (
				construct (yesN, yesL)
				, construct (noN, noL)
			)
			fun loop (i, yesN, yesL, noN, noL) =
				if i < 0 then return (yesN, yesL, noN, noL)
				else
					let
						val x = T.sub (t, i)
					in
						if f x then loop (
							i - 1
							, yesN + 1, x :: yesL
							, noN, noL
						) else loop (
							i - 1
							, yesN, yesL
							, noN + 1, x :: noL
						)
					end
		in
			loop (n - 1, 0, nil, 0, nil)
		end
	fun concatWith t l =
		let
			fun loop (l, r) = case l of
				nil => empty ()
				| x :: nil => T.concat (List.rev (x :: l))
				| x :: (y as (z :: _)) => loop (y, z :: t :: r)
		in
			loop (l, nil)
		end
end
