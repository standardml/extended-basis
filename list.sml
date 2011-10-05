(*
	Lists
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
signature LIST = sig
	type 'a t = 'a list
	include CREATEABLE_SEQUENCE where type 'a sequence = 'a t
	include PURE_SEQUENCE where type 'a pureSequence = 'a t
	val @: 'a t * 'a t -> 'a t
	val nth: 'a t * int -> 'a
	val revAppend: 'a t * 'a t -> 'a t
end

structure List :> EXTENDED_LIST = struct
	type 'a t = 'a list
	type 'a sequence = 'a t
	fun isEmpty t = case t of nil => true | _ => false
	val length = List.length
	val hd = List.hd
	val sub = List.nth
	val appl = List.app
	val app = List.app
	fun appli f l =
		let
			fun loop (i, l) = case l of
				nil => ()
				| x :: y => (
					f (i, x)
					; loop (i + 1, y)
				)
		in
			loop (0, l)
		end
	val appi = appli
	fun appr f l = case l of
		nil => ()
		| x :: y => (
			appri f y
			; f x
		)
	fun appri f l =
		let
			fun loop (i, l) = case l of
				nil => ()
				| x :: y => (
					loop (i + 1, y)
					; f (i, x)
				)
		in
			loop (0, l)
		end
	val foldl = List.foldl
	val fold = List.foldl
	val foldli = List.foldli
	val foldi = List.foldli
	val foldr = List.foldr
	val foldri = List.foldri
	fun reducel f l = case l of
		nil => raise Empty
		| x :: nil => x
		| x :: y => f (reducel f y, x)
	val reduce = reducel
	fun reducer f l = case l of
		nil => raise Empty
		| x :: nil => x
		| x :: y => f (x, reducer f y)
	val findl = List.find
	val find = List.find
	val findli = List.findi
	val findi = List.findi
	fun findr f l = findl f (List.rev l)
	fun findri f l =
		let
			fun loop (i, l, m) = case m of
				nil => (i, l)
				| x :: y => loop (i + 1, x :: l, y)
			val (n, l) = loop (0, nil, l)
			fun loop (i, l) = case l of
				nil => NONE
				| x :: y =>
					let
						val z = (i, x)
					in
						if f z then SOME z
						else loop (i - 1, y)
					end
		in
			loop (n - 1, l)
		end
	val exists = List.exists
	val all = List.all
	val collate = List.collate
	val toShadow = Shadow.fromList
	val revToShadow = Shadow.fromRevList
	fun toList l = l
	val revToList = List.rev
	val toArray = Array.fromList
	fun revToArray l = Array.fromList (List.rev l)
	val toCharArray = CharArray.fromList
	fun revToCharArray l = CharArray.fromList (List.rev l)
	val toBoolArray = BoolArray.fromList
	fun revToBoolArray l = BoolArray.fromList (List.rev l)
	val toIntArray = IntArray.fromList
	fun revToIntArray l = IntArray.fromList (List.rev l)
	val toWordArray = WordArray.fromList
	fun revToWordArray l = WordArray.fromList (List.rev l)
	val toRealArray = RealArray.fromList
	fun revToRealArray l = RealArray.fromList (List.rev l)
	val toLargeIntArray = LargeIntArray.fromList
	fun revToLargeIntArray l = LargeIntArray.fromList (List.rev l)
	val toLargeWordArray = LargeWordArray.fromList
	fun revToLargeWordArray l = LargeWordArray.fromList (List.rev l)
	val toInt8Array = Int8Array.fromList
	fun revToInt8Array l = Int8Array.fromList (List.rev l)
	val toInt16Array = Int16Array.fromList
	fun revToInt16Array l = Int16Array.fromList (List.rev l)
	val toInt32Array = Int32Array.fromList
	fun revToInt32Array l = Int32Array.fromList (List.rev l)
	val toInt64Array = Int64Array.fromList
	fun revToInt64Array l = Int64Array.fromList (List.rev l)
	val toWord8Array = Word8Array.fromList
	fun revToWord8Array l = Word8Array.fromList (List.rev l)
	val toWord16Array = Word16Array.fromList
	fun revToWord16Array l = Word16Array.fromList (List.rev l)
	val toWord32Array = Word32Array.fromList
	fun revToWord32Array l = Word32Array.fromList (List.rev l)
	val toWord64Array = Word64Array.fromList
	fun revToWord64Array l = Word64Array.fromList (List.rev l)
	val toReal32Array = Real32Array.fromList
	fun revToReal32Array l = Real32Array.fromList (List.rev l)
	val toReal64Array = Real64Array.fromList
	fun revToReal64Array l = Real64Array.fromList (List.rev l)
	val toVector = Vector.fromList
	fun revToVector l = Vector.fromList (List.rev l)
	val toCharVector = CharVector.fromList
	fun revToCharVector l = CharVector.fromList (List.rev l)
	val toBoolVector = BoolVector.fromList
	fun revToBoolVector l = BoolVector.fromList (List.rev l)
	val toIntVector = IntVector.fromList
	fun revToIntVector l = IntVector.fromList (List.rev l)
	val toWordVector = WordVector.fromList
	fun revToWordVector l = WordVector.fromList (List.rev l)
	val toRealVector = RealVector.fromList
	fun revToRealVector l = RealVector.fromList (List.rev l)
	val toLargeIntVector = LargeIntVector.fromList
	fun revToLargeIntVector l = LargeIntVector.fromList (List.rev l)
	val toLargeWordVector = LargeWordVector.fromList
	fun revToLargeWordVector l = LargeWordVector.fromList (List.rev l)
	val toInt8Vector = Int8Vector.fromList
	fun revToInt8Vector l = Int8Vector.fromList (List.rev l)
	val toInt16Vector = Int16Vector.fromList
	fun revToInt16Vector l = Int16Vector.fromList (List.rev l)
	val toInt32Vector = Int32Vector.fromList
	fun revToInt32Vector l = Int32Vector.fromList (List.rev l)
	val toInt64Vector = Int64Vector.fromList
	fun revToInt64Vector l = Int64Vector.fromList (List.rev l)
	val toWord8Vector = Word8Vector.fromList
	fun revToWord8Vector l = Word8Vector.fromList (List.rev l)
	val toWord16Vector = Word16Vector.fromList
	fun revToWord16Vector l = Word16Vector.fromList (List.rev l)
	val toWord32Vector = Word32Vector.fromList
	fun revToWord32Vector l = Word32Vector.fromList (List.rev l)
	val toWord64Vector = Word64Vector.fromList
	fun revToWord64Vector l = Word64Vector.fromList (List.rev l)
	val toReal32Vector = Real32Vector.fromList
	fun revToReal32Vector l = Real32Vector.fromList (List.rev l)
	val toReal64Vector = Real64Vector.fromList
	fun revToReal64Vector l = Real64Vector.fromList (List.rev l)
	val toString = String.implode
	fun revToString l = String.implode (List.rev l)
	val maxLen = Int.maxInt
	fun empty () = nil
	fun single x = x :: nil
	fun unfold f a = case f a of
		NONE => nil
		| SOME (b, a) => b :: (unfold f a)
	fun unfoldn f (n, a) =
		if n = 0 then nil
		else
			let
				val (b, a) = f a
			in
				b :: (unfoldn f (n - 1, a))
			end
	val tabulate = List.tabulate
	fun split f l =
		let
			fun loop (l, m) = case m of
				nil => (List.rev l, nil)
				| x :: y =>
					if f x then loop (x :: l, y)
					else (List.rev (x :: l), y)
		in
			loop (nil, l)
		end
	fun splitAt (l, n) =
		let
			fun loop (i, l, m) =
				if i = n then (List.rev l, m)
				else case m of
					nil => raise Subscript
					| x :: y => loop (i + 1, x :: l, y)
		in
			loop (0, nil, l)
		end
	val drop = 
end
