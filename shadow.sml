(*
	Shadows - Abstract, lazy, functional sequences
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

signature SHADOW = sig
	(*
		'a t is the type of a shadow - a sequence containing elements of
		type 'a. Retrieval is generally lazy, meaning that an
		element is not retrieved unless and until it is requested.
		For shadows returned from map, filter, etc. this also
		means that the given function is not applied to an element
		unless and until the result of that application is requested.
		Shadows can be based on either random or sequential access,
		which will determine the performance, but not the availability,
		of operations.
	*)
	type 'a t = 'a Shadow.t

	(*
		Shadows are createable and pure.
	*)
	include CREATEABLE_SEQUENCE where type 'a sequence = 'a t
	include PURE_SEQUENCE where type 'a pureSequence = 'a t

	(*
		This exception is raised when a shadow returns an erroneous
		length, that is, the number of elements returned is not
		the same as the purported length.
	*)
	exception BadLength

	(*
		This exception is raised when the length of a shadow is
		requested, but the shadow does not have a finite length.
	*)
	exception InfiniteLength

	datatype access = datatype Shadow.access

	(*
		access t returns the access characteristics of t.
	*)
	val access: 'a t -> access

	(*
		create {access, getItem, sub, length} creates a sequence from
		an access indicator and three functions.
		access indicates whether the underlying structure of a shadow
		is random or sequential.
		getItem is a function which returns either NONE if the
		sequence is empty, or SOME (x, t) in which x is the next
		element in the sequence and t is a sequence containing
		the remaining elements. next must be idempotent, that
		is, repeated calls to next must have the same effect as
		one call.
		sub is a function which returns an element by index. If
		the given index is greater than or equal to the length of
		the shadow, sub should raise the Subscript exception.
		length is a function which returns the number of
		elements in the sequence. If the sequence does not have
		a finite number of elements, then length should raise
		InfiniteLength.
	*)
	val create: {
		access: access
		, getItem: unit -> ('a * 'a t) option
		, sub: int -> 'a
		, length: unit -> int
	} -> 'a t

	(*
		fromRandom {sub, length} creates a shadow from two functions.
		This is equivalent to create above, except that access is
		assumed to be Random and getItem is computed using sub and length.
	*)
	val fromRandom: {sub: int -> 'a, length: unit -> int} -> 'a t

	(*
		fromIdempotentFunction f creates a shadow from a function.
		This is equivalent to create above, giving f as getItem, except
		that access is assumed to be Sequential, and sub and length are
		computed by calling f.
	*)
	val fromIdempotentFunction: (unit -> ('a * 'a t) option) -> 'a t

	(*
		fromFunction f creates a sequence from a function.
		This is equivalent to fromIdempotentFunction above, except
		that f is not required to be idempotent; f will only ever
		be called zero or one times.
	*)
	val fromFunction: (unit -> ('a * 'a t) option) -> 'a t


	(*
		for {start, test, step} returns a sequence which
		goes through a sequence of values. The first value
		is given by start. On each iteration, test is applied,
		and the value is yielded only if test returns true.
		If test start returns false, then the sequence is empty.
		The next value is given by applying step to the previous
		value. This behavior mimics the for loops in C, C++, and
		Java.
	*)
	val for: {
		start: 'a
		, test: 'a -> bool
		, step: 'a -> 'a
	} -> 'a t

	(*
		numbers {from, to, step} returns a sequence of numbers
		starting at from, ending inclusively at to, with an
		increment of step. step may be negative, or even zero. If
		the difference between from and to is not a multiple of
		step, the last element in the sequence will not be to.
		If step is zero, requesting the length of the sequence
		will result in raising InfiniteLength. If step is
		negative and to is greather than from, or step is positive
		and to is less than from, the sequence will be empty.
	*)
	val numbers: {from: int, to: int, step: int} -> int t

	(*
		fromReader f a returns a sequence based on applying
		f to a. The result of f a should be either NONE, indicating
		the end of the sequence, or SOME (x, b), in which x
		is the next element of the sequence and f b is the next
		application. f must be idempotent, that is, multiple calls
		to f a, f b, etc. should be the same as one call. fromReader
		could be used with many functions from the Standard Basis,
		including TextIO.StreamIO.input1, Substring.getc,
		VectorSlice.getItem, List.getItem, etc., although
		conversion functions have been provided, which in some cases
		give a faster length.
	*)
	val fromReader: ('a -> ('b * 'a) option) -> 'a -> 'b t

	(*
		pairs t returns a sequence of pairs of the elements of
		t. The first pair contains the first and second element,
		the second pair contains the third and fourth element,
		and so on. If t contains an odd number of elements,
		the last element is ignored.
	*)
	val pairs: 'a t -> ('a * 'a) t

	(*
		group (t, n) returns a sequence of subsequences,
		in which each subsequence has a length of n elements,
		taken in succession from t.
	*)
	val group: ('a t * int) -> 'a t t
end

structure Shadow :> SHADOW = struct
	(*
		Because the SHADOW signature includes CREATEABLE_SEQUENCE etc.
		which themselves require the Shadow.t type, Shadow.t (and Shadow.access,
		upon which it depends) is defined elsewhere and then opened here.
	*)
	open Shadow
	type 'a sequence = 'a t
	type 'a pureSequence = 'a t
	val maxLen = valOf Int.maxInt
	exception BadLength
	exception InfiniteLength
	fun create x = T x
	fun access (T {access, ...}) = access
	fun sub (T {sub, ...}, i) = sub i
	fun getItem (T {getItem, ...}) = getItem ()
	fun length (T {length, ...}) = length ()
	fun empty () = create {
		access = Random
		, getItem = fn () => NONE
		, length = fn () => 0
		, sub = fn _ => raise Subscript
	}
	fun single x = create {
		access = Random
		, getItem = fn () => SOME (
			x
			, empty ()
		), length = fn () => 1
		, sub = fn
			0 => x
			| _ => raise Subscript
	}
	fun hd t = case getItem t of
		NONE => raise Empty
		| SOME (x, _) => x
	fun tl t = case getItem t of
		NONE => raise Empty
		| SOME (_, x) => x
	fun fromRandom {sub, length} =
		let
			fun length' offset () = length () - offset
			fun sub' offset index = sub (offset + index)
			fun getItem' offset () =
				if offset >= length () then NONE
				else SOME (
					sub offset
					, create' (offset + 1)
				)
			and create' offset = create {
				access = Random
				, sub = sub' offset
				, length = length' offset
				, getItem = getItem' offset
			}
		in
			create' 0
		end
	fun fromIdempotentFunction f =
		let
			fun length' () = case f () of
				NONE => 0
				| SOME (_, x) => 1 + length x
			fun sub' i = case (i, f ()) of
				(_, NONE) => raise Subscript
				| (0, SOME (x, _)) => x
				| (_, SOME (_, x)) => sub (x, i - 1)
		in
			create {
				access = Sequential
				, getItem = f
				, length = length'
				, sub = sub'
			}
		end
	fun lazy f =
		let
			val x = ref NONE
		in
			fn () => (case !x of
				NONE =>
					let
						val y = f ()
					in
						x := SOME y
						; y
					end
				| SOME y => y
			)
		end
	fun fromFunction f = fromIdempotentFunction (lazy f)
	fun unfold f a = fromFunction (fn () =>
		case f a of
			NONE => NONE
			| SOME (b, a) => SOME (
				b
				, unfold f a
			)
	)
	fun unfoldn f (n, a) = fromFunction (fn () =>
		if n <= 0 then NONE
		else
			let
				val (b, a) = f a
			in
				SOME (b, unfoldn f (n - 1, a))
			end
	)
	fun for {start, test, step} = fromFunction (fn () =>
		if test start then SOME (
			start
			, for {start = step start, test = test, step = step}
		) else NONE
	)
	fun numbers {from, to, step} =
		let
			fun zero () = create {
				access = Random
				, getItem = fn () => SOME (from, zero ())
				, length = fn () => raise InfiniteLength
				, sub = fn _ => from
			}
			fun length current () =
				Int.max ((to - current) div step + 1, 0)
			fun nonzero compare current = create {
				access = Random
				, length = length current
				, getItem = fn () =>
					if compare (current, to) then SOME (
						current
						, nonzero compare (step + current)
					) else NONE
				, sub = fn i =>
					let
						val target = current + step * i
					in
						if compare (target, to) then target
						else raise Subscript
					end
			}
		in
			case Int.compare (step, 0) of
				EQUAL =>
					if from = to then single from
					else zero ()
				| GREATER => nonzero (op <=) from
				| LESS => nonzero (op >=) from
		end
	fun fromShadow t = t
	fun fromReader f a = fromIdempotentFunction (fn () =>
		(case f a of
			NONE => NONE
			| SOME (b, a) => SOME (b, fromReader f a)
		)
	)
	fun fromSlice (x as (length, getItem, sub)) slice = create {
		access = Random
		, length = fn () => length slice
		, getItem = fn () => (
			case getItem slice of
				NONE => NONE
				| SOME (y, slice) => SOME (
					y
					, fromSlice x slice
				)
		), sub = fn i => sub (slice, i)
	}
	fun fromRevSlice (x as (length, sub, subslice)) slice = create {
		access = Random
		, length = fn () => length slice
		, getItem = fn () =>
			let
				val length' = length slice
			in
				if length' = 0 then NONE
				else SOME (
					sub (slice, length' - 1)
					, fromRevSlice x (subslice (slice, 0, SOME (length' - 1)))
				)
			end
		, sub = fn index => sub (slice, index)
	}
	fun fromArraySlice slice =
		fromSlice (
			ArraySlice.length
			, ArraySlice.getItem
			, ArraySlice.sub
		) slice
	fun fromRevArraySlice slice =
		fromRevSlice (
			ArraySlice.length
			, ArraySlice.sub
			, ArraySlice.subslice
		) slice
	val fromCharArraySlice =
		fromSlice (
			CharArraySlice.length
			, CharArraySlice.getItem
			, CharArraySlice.sub
		)
	val fromRevCharArraySlice =
		fromRevSlice (
			CharArraySlice.length
			, CharArraySlice.sub
			, CharArraySlice.subslice
		)
	val fromBoolArraySlice =
		fromSlice (
			BoolArraySlice.length
			, BoolArraySlice.getItem
			, BoolArraySlice.sub
		)
	val fromRevBoolArraySlice =
		fromRevSlice (
			BoolArraySlice.length
			, BoolArraySlice.sub
			, BoolArraySlice.subslice
		)
	val fromIntArraySlice =
		fromSlice (
			IntArraySlice.length
			, IntArraySlice.getItem
			, IntArraySlice.sub
		)
	val fromRevIntArraySlice =
		fromRevSlice (
			IntArraySlice.length
			, IntArraySlice.sub
			, IntArraySlice.subslice
		)
	val fromWordArraySlice =
		fromSlice (
			WordArraySlice.length
			, WordArraySlice.getItem
			, WordArraySlice.sub
		)
	val fromRevWordArraySlice =
		fromRevSlice (
			WordArraySlice.length
			, WordArraySlice.sub
			, WordArraySlice.subslice
		)
	val fromRealArraySlice =
		fromSlice (
			RealArraySlice.length
			, RealArraySlice.getItem
			, RealArraySlice.sub
		)
	val fromRevRealArraySlice =
		fromRevSlice (
			RealArraySlice.length
			, RealArraySlice.sub
			, RealArraySlice.subslice
		)
	val fromLargeIntArraySlice =
		fromSlice (
			LargeIntArraySlice.length
			, LargeIntArraySlice.getItem
			, LargeIntArraySlice.sub
		)
	val fromRevLargeIntArraySlice =
		fromRevSlice (
			LargeIntArraySlice.length
			, LargeIntArraySlice.sub
			, LargeIntArraySlice.subslice
		)
	val fromLargeWordArraySlice =
		fromSlice (
			LargeWordArraySlice.length
			, LargeWordArraySlice.getItem
			, LargeWordArraySlice.sub
		)
	val fromRevLargeWordArraySlice =
		fromRevSlice (
			LargeWordArraySlice.length
			, LargeWordArraySlice.sub
			, LargeWordArraySlice.subslice
		)
	val fromInt8ArraySlice =
		fromSlice (
			Int8ArraySlice.length
			, Int8ArraySlice.getItem
			, Int8ArraySlice.sub
		)
	val fromRevInt8ArraySlice =
		fromRevSlice (
			Int8ArraySlice.length
			, Int8ArraySlice.sub
			, Int8ArraySlice.subslice
		)
	val fromInt16ArraySlice =
		fromSlice (
			Int16ArraySlice.length
			, Int16ArraySlice.getItem
			, Int16ArraySlice.sub
		)
	val fromRevInt16ArraySlice =
		fromRevSlice (
			Int16ArraySlice.length
			, Int16ArraySlice.sub
			, Int16ArraySlice.subslice
		)
	val fromInt32ArraySlice =
		fromSlice (
			Int32ArraySlice.length
			, Int32ArraySlice.getItem
			, Int32ArraySlice.sub
		)
	val fromRevInt32ArraySlice =
		fromRevSlice (
			Int32ArraySlice.length
			, Int32ArraySlice.sub
			, Int32ArraySlice.subslice
		)
	val fromInt64ArraySlice =
		fromSlice (
			Int64ArraySlice.length
			, Int64ArraySlice.getItem
			, Int64ArraySlice.sub
		)
	val fromRevInt64ArraySlice =
		fromRevSlice (
			Int64ArraySlice.length
			, Int64ArraySlice.sub
			, Int64ArraySlice.subslice
		)
	val fromWord8ArraySlice =
		fromSlice (
			Word8ArraySlice.length
			, Word8ArraySlice.getItem
			, Word8ArraySlice.sub
		)
	val fromRevWord8ArraySlice =
		fromRevSlice (
			Word8ArraySlice.length
			, Word8ArraySlice.sub
			, Word8ArraySlice.subslice
		)
	val fromWord16ArraySlice =
		fromSlice (
			Word16ArraySlice.length
			, Word16ArraySlice.getItem
			, Word16ArraySlice.sub
		)
	val fromRevWord16ArraySlice =
		fromRevSlice (
			Word16ArraySlice.length
			, Word16ArraySlice.sub
			, Word16ArraySlice.subslice
		)
	val fromWord32ArraySlice =
		fromSlice (
			Word32ArraySlice.length
			, Word32ArraySlice.getItem
			, Word32ArraySlice.sub
		)
	val fromRevWord32ArraySlice =
		fromRevSlice (
			Word32ArraySlice.length
			, Word32ArraySlice.sub
			, Word32ArraySlice.subslice
		)
	val fromWord64ArraySlice =
		fromSlice (
			Word64ArraySlice.length
			, Word64ArraySlice.getItem
			, Word64ArraySlice.sub
		)
	val fromRevWord64ArraySlice =
		fromRevSlice (
			Word64ArraySlice.length
			, Word64ArraySlice.sub
			, Word64ArraySlice.subslice
		)
	val fromReal32ArraySlice =
		fromSlice (
			Real32ArraySlice.length
			, Real32ArraySlice.getItem
			, Real32ArraySlice.sub
		)
	val fromRevReal32ArraySlice =
		fromRevSlice (
			Real32ArraySlice.length
			, Real32ArraySlice.sub
			, Real32ArraySlice.subslice
		)
	val fromReal64ArraySlice =
		fromSlice (
			Real64ArraySlice.length
			, Real64ArraySlice.getItem
			, Real64ArraySlice.sub
		)
	val fromRevReal64ArraySlice =
		fromRevSlice (
			Real64ArraySlice.length
			, Real64ArraySlice.sub
			, Real64ArraySlice.subslice
		)
	fun fromArray array = fromArraySlice (ArraySlice.full array)
	fun fromRevArray array = fromRevArraySlice (ArraySlice.full array)
	val fromCharArray = fromCharArraySlice o CharArraySlice.full
	val fromRevCharArray = fromRevCharArraySlice o CharArraySlice.full
	val fromBoolArray = fromBoolArraySlice o BoolArraySlice.full
	val fromRevBoolArray = fromRevBoolArraySlice o BoolArraySlice.full
	val fromIntArray = fromIntArraySlice o IntArraySlice.full
	val fromRevIntArray = fromRevIntArraySlice o IntArraySlice.full
	val fromWordArray = fromWordArraySlice o WordArraySlice.full
	val fromRevWordArray = fromRevWordArraySlice o WordArraySlice.full
	val fromRealArray = fromRealArraySlice o RealArraySlice.full
	val fromRevRealArray = fromRevRealArraySlice o RealArraySlice.full
	val fromLargeIntArray = fromLargeIntArraySlice o LargeIntArraySlice.full
	val fromRevLargeIntArray = fromRevLargeIntArraySlice o LargeIntArraySlice.full
	val fromLargeWordArray = fromLargeWordArraySlice o LargeWordArraySlice.full
	val fromRevLargeWordArray = fromRevLargeWordArraySlice o LargeWordArraySlice.full
	val fromInt8Array = fromInt8ArraySlice o Int8ArraySlice.full
	val fromRevInt8Array = fromRevInt8ArraySlice o Int8ArraySlice.full
	val fromInt16Array = fromInt16ArraySlice o Int16ArraySlice.full
	val fromRevInt16Array = fromRevInt16ArraySlice o Int16ArraySlice.full
	val fromInt32Array = fromInt32ArraySlice o Int32ArraySlice.full
	val fromRevInt32Array = fromRevInt32ArraySlice o Int32ArraySlice.full
	val fromInt64Array = fromInt64ArraySlice o Int64ArraySlice.full
	val fromRevInt64Array = fromRevInt64ArraySlice o Int64ArraySlice.full
	val fromWord8Array = fromWord8ArraySlice o Word8ArraySlice.full
	val fromRevWord8Array = fromRevWord8ArraySlice o Word8ArraySlice.full
	val fromWord16Array = fromWord16ArraySlice o Word16ArraySlice.full
	val fromRevWord16Array = fromRevWord16ArraySlice o Word16ArraySlice.full
	val fromWord32Array = fromWord32ArraySlice o Word32ArraySlice.full
	val fromRevWord32Array = fromRevWord32ArraySlice o Word32ArraySlice.full
	val fromWord64Array = fromWord64ArraySlice o Word64ArraySlice.full
	val fromRevWord64Array = fromRevWord64ArraySlice o Word64ArraySlice.full
	val fromReal32Array = fromReal32ArraySlice o Real32ArraySlice.full
	val fromRevReal32Array = fromRevReal32ArraySlice o Real32ArraySlice.full
	val fromReal64Array = fromReal64ArraySlice o Real64ArraySlice.full
	val fromRevReal64Array = fromRevReal64ArraySlice o Real64ArraySlice.full
	fun fromVectorSlice slice =
		fromSlice (
			VectorSlice.length
			, VectorSlice.getItem
			, VectorSlice.sub
		) slice
	fun fromRevVectorSlice slice =
		fromRevSlice (
			VectorSlice.length
			, VectorSlice.sub
			, VectorSlice.subslice
		) slice
	val fromCharVectorSlice =
		fromSlice (
			CharVectorSlice.length
			, CharVectorSlice.getItem
			, CharVectorSlice.sub
		)
	val fromRevCharVectorSlice =
		fromRevSlice (
			CharVectorSlice.length
			, CharVectorSlice.sub
			, CharVectorSlice.subslice
		)
	val fromBoolVectorSlice =
		fromSlice (
			BoolVectorSlice.length
			, BoolVectorSlice.getItem
			, BoolVectorSlice.sub
		)
	val fromRevBoolVectorSlice =
		fromRevSlice (
			BoolVectorSlice.length
			, BoolVectorSlice.sub
			, BoolVectorSlice.subslice
		)
	val fromIntVectorSlice =
		fromSlice (
			IntVectorSlice.length
			, IntVectorSlice.getItem
			, IntVectorSlice.sub
		)
	val fromRevIntVectorSlice =
		fromRevSlice (
			IntVectorSlice.length
			, IntVectorSlice.sub
			, IntVectorSlice.subslice
		)
	val fromWordVectorSlice =
		fromSlice (
			WordVectorSlice.length
			, WordVectorSlice.getItem
			, WordVectorSlice.sub
		)
	val fromRevWordVectorSlice =
		fromRevSlice (
			WordVectorSlice.length
			, WordVectorSlice.sub
			, WordVectorSlice.subslice
		)
	val fromRealVectorSlice =
		fromSlice (
			RealVectorSlice.length
			, RealVectorSlice.getItem
			, RealVectorSlice.sub
		)
	val fromRevRealVectorSlice =
		fromRevSlice (
			RealVectorSlice.length
			, RealVectorSlice.sub
			, RealVectorSlice.subslice
		)
	val fromLargeIntVectorSlice =
		fromSlice (
			LargeIntVectorSlice.length
			, LargeIntVectorSlice.getItem
			, LargeIntVectorSlice.sub
		)
	val fromRevLargeIntVectorSlice =
		fromRevSlice (
			LargeIntVectorSlice.length
			, LargeIntVectorSlice.sub
			, LargeIntVectorSlice.subslice
		)
	val fromLargeWordVectorSlice =
		fromSlice (
			LargeWordVectorSlice.length
			, LargeWordVectorSlice.getItem
			, LargeWordVectorSlice.sub
		)
	val fromRevLargeWordVectorSlice =
		fromRevSlice (
			LargeWordVectorSlice.length
			, LargeWordVectorSlice.sub
			, LargeWordVectorSlice.subslice
		)
	val fromInt8VectorSlice =
		fromSlice (
			Int8VectorSlice.length
			, Int8VectorSlice.getItem
			, Int8VectorSlice.sub
		)
	val fromRevInt8VectorSlice =
		fromRevSlice (
			Int8VectorSlice.length
			, Int8VectorSlice.sub
			, Int8VectorSlice.subslice
		)
	val fromInt16VectorSlice =
		fromSlice (
			Int16VectorSlice.length
			, Int16VectorSlice.getItem
			, Int16VectorSlice.sub
		)
	val fromRevInt16VectorSlice =
		fromRevSlice (
			Int16VectorSlice.length
			, Int16VectorSlice.sub
			, Int16VectorSlice.subslice
		)
	val fromInt32VectorSlice =
		fromSlice (
			Int32VectorSlice.length
			, Int32VectorSlice.getItem
			, Int32VectorSlice.sub
		)
	val fromRevInt32VectorSlice =
		fromRevSlice (
			Int32VectorSlice.length
			, Int32VectorSlice.sub
			, Int32VectorSlice.subslice
		)
	val fromInt64VectorSlice =
		fromSlice (
			Int64VectorSlice.length
			, Int64VectorSlice.getItem
			, Int64VectorSlice.sub
		)
	val fromRevInt64VectorSlice =
		fromRevSlice (
			Int64VectorSlice.length
			, Int64VectorSlice.sub
			, Int64VectorSlice.subslice
		)
	val fromWord8VectorSlice =
		fromSlice (
			Word8VectorSlice.length
			, Word8VectorSlice.getItem
			, Word8VectorSlice.sub
		)
	val fromRevWord8VectorSlice =
		fromRevSlice (
			Word8VectorSlice.length
			, Word8VectorSlice.sub
			, Word8VectorSlice.subslice
		)
	val fromWord16VectorSlice =
		fromSlice (
			Word16VectorSlice.length
			, Word16VectorSlice.getItem
			, Word16VectorSlice.sub
		)
	val fromRevWord16VectorSlice =
		fromRevSlice (
			Word16VectorSlice.length
			, Word16VectorSlice.sub
			, Word16VectorSlice.subslice
		)
	val fromWord32VectorSlice =
		fromSlice (
			Word32VectorSlice.length
			, Word32VectorSlice.getItem
			, Word32VectorSlice.sub
		)
	val fromRevWord32VectorSlice =
		fromRevSlice (
			Word32VectorSlice.length
			, Word32VectorSlice.sub
			, Word32VectorSlice.subslice
		)
	val fromWord64VectorSlice =
		fromSlice (
			Word64VectorSlice.length
			, Word64VectorSlice.getItem
			, Word64VectorSlice.sub
		)
	val fromRevWord64VectorSlice =
		fromRevSlice (
			Word64VectorSlice.length
			, Word64VectorSlice.sub
			, Word64VectorSlice.subslice
		)
	val fromReal32VectorSlice =
		fromSlice (
			Real32VectorSlice.length
			, Real32VectorSlice.getItem
			, Real32VectorSlice.sub
		)
	val fromRevReal32VectorSlice =
		fromRevSlice (
			Real32VectorSlice.length
			, Real32VectorSlice.sub
			, Real32VectorSlice.subslice
		)
	val fromReal64VectorSlice =
		fromSlice (
			Real64VectorSlice.length
			, Real64VectorSlice.getItem
			, Real64VectorSlice.sub
		)
	val fromRevReal64VectorSlice =
		fromRevSlice (
			Real64VectorSlice.length
			, Real64VectorSlice.sub
			, Real64VectorSlice.subslice
		)
	fun fromVector vector = fromVectorSlice (VectorSlice.full vector)
	fun fromRevVector vector = fromRevVectorSlice (VectorSlice.full vector)
	val fromCharVector = fromCharVectorSlice o CharVectorSlice.full
	val fromRevCharVector = fromRevCharVectorSlice o CharVectorSlice.full
	val fromBoolVector = fromBoolVectorSlice o BoolVectorSlice.full
	val fromRevBoolVector = fromRevBoolVectorSlice o BoolVectorSlice.full
	val fromIntVector = fromIntVectorSlice o IntVectorSlice.full
	val fromRevIntVector = fromRevIntVectorSlice o IntVectorSlice.full
	val fromWordVector = fromWordVectorSlice o WordVectorSlice.full
	val fromRevWordVector = fromRevWordVectorSlice o WordVectorSlice.full
	val fromRealVector = fromRealVectorSlice o RealVectorSlice.full
	val fromRevRealVector = fromRevRealVectorSlice o RealVectorSlice.full
	val fromLargeIntVector = fromLargeIntVectorSlice o LargeIntVectorSlice.full
	val fromRevLargeIntVector = fromRevLargeIntVectorSlice o LargeIntVectorSlice.full
	val fromLargeWordVector = fromLargeWordVectorSlice o LargeWordVectorSlice.full
	val fromRevLargeWordVector = fromRevLargeWordVectorSlice o LargeWordVectorSlice.full
	val fromInt8Vector = fromInt8VectorSlice o Int8VectorSlice.full
	val fromRevInt8Vector = fromRevInt8VectorSlice o Int8VectorSlice.full
	val fromInt16Vector = fromInt16VectorSlice o Int16VectorSlice.full
	val fromRevInt16Vector = fromRevInt16VectorSlice o Int16VectorSlice.full
	val fromInt32Vector = fromInt32VectorSlice o Int32VectorSlice.full
	val fromRevInt32Vector = fromRevInt32VectorSlice o Int32VectorSlice.full
	val fromInt64Vector = fromInt64VectorSlice o Int64VectorSlice.full
	val fromRevInt64Vector = fromRevInt64VectorSlice o Int64VectorSlice.full
	val fromWord8Vector = fromWord8VectorSlice o Word8VectorSlice.full
	val fromRevWord8Vector = fromRevWord8VectorSlice o Word8VectorSlice.full
	val fromWord16Vector = fromWord16VectorSlice o Word16VectorSlice.full
	val fromRevWord16Vector = fromRevWord16VectorSlice o Word16VectorSlice.full
	val fromWord32Vector = fromWord32VectorSlice o Word32VectorSlice.full
	val fromRevWord32Vector = fromRevWord32VectorSlice o Word32VectorSlice.full
	val fromWord64Vector = fromWord64VectorSlice o Word64VectorSlice.full
	val fromRevWord64Vector = fromRevWord64VectorSlice o Word64VectorSlice.full
	val fromReal32Vector = fromReal32VectorSlice o Real32VectorSlice.full
	val fromRevReal32Vector = fromRevReal32VectorSlice o Real32VectorSlice.full
	val fromReal64Vector = fromReal64VectorSlice o Real64VectorSlice.full
	val fromRevReal64Vector = fromRevReal64VectorSlice o Real64VectorSlice.full
	val fromString = fromCharVector
	val fromRevString = fromRevCharVector
	val fromSubstring = fromCharVectorSlice
	val fromRevSubstring = fromRevCharVectorSlice
	fun fromList list = fromReader List.getItem list
	fun fromTextIO instream = fromFunction (
		fn () => (case TextIO.input1 instream of
			NONE => NONE
			| SOME x => SOME (
				x
				, fromTextIO instream
			)
		)
	)
	fun fromTextIOLines instream = fromFunction (
		fn () => (case TextIO.inputLine instream of
			NONE => NONE
			| SOME x => SOME (
				x
				, fromTextIOLines instream
			)
		)
	)
	fun fromBinIO instream = fromFunction (
		fn () => (case BinIO.input1 instream of
			NONE => NONE
			| SOME x => SOME (
				x
				, fromBinIO instream
			)
		)
	)
	fun toTabulated tabulate t =
		let
			val t = ref t
		in
			tabulate (
				length (!t)
				, fn _ => (case getItem (!t) of
					NONE => raise BadLength
					| SOME (x, u) => (
						t := u
						; x
					)
				)
			)
		end
	fun toArray t = toTabulated Array.tabulate t
	val toCharArray = toTabulated CharArray.tabulate
	val toBoolArray = toTabulated BoolArray.tabulate
	val toIntArray = toTabulated IntArray.tabulate
	val toWordArray = toTabulated WordArray.tabulate
	val toRealArray = toTabulated RealArray.tabulate
	val toLargeIntArray = toTabulated LargeIntArray.tabulate
	val toLargeWordArray = toTabulated LargeWordArray.tabulate
	val toInt8Array = toTabulated Int8Array.tabulate
	val toInt16Array = toTabulated Int16Array.tabulate
	val toInt32Array = toTabulated Int32Array.tabulate
	val toInt64Array = toTabulated Int64Array.tabulate
	val toWord8Array = toTabulated Word8Array.tabulate
	val toWord16Array = toTabulated Word16Array.tabulate
	val toWord32Array = toTabulated Word32Array.tabulate
	val toWord64Array = toTabulated Word64Array.tabulate
	val toReal32Array = toTabulated Real32Array.tabulate
	val toReal64Array = toTabulated Real64Array.tabulate
	fun toVector t = toTabulated Vector.tabulate t
	val toCharVector = toTabulated CharVector.tabulate
	val toBoolVector = toTabulated BoolVector.tabulate
	val toIntVector = toTabulated IntVector.tabulate
	val toWordVector = toTabulated WordVector.tabulate
	val toRealVector = toTabulated RealVector.tabulate
	val toLargeIntVector = toTabulated LargeIntVector.tabulate
	val toLargeWordVector = toTabulated LargeWordVector.tabulate
	val toInt8Vector = toTabulated Int8Vector.tabulate
	val toInt16Vector = toTabulated Int16Vector.tabulate
	val toInt32Vector = toTabulated Int32Vector.tabulate
	val toInt64Vector = toTabulated Int64Vector.tabulate
	val toWord8Vector = toTabulated Word8Vector.tabulate
	val toWord16Vector = toTabulated Word16Vector.tabulate
	val toWord32Vector = toTabulated Word32Vector.tabulate
	val toWord64Vector = toTabulated Word64Vector.tabulate
	val toReal32Vector = toTabulated Real32Vector.tabulate
	val toReal64Vector = toTabulated Real64Vector.tabulate
	val toString = toCharVector
	fun rev t = case access t of
		Sequential => fromRevVector (toVector t)
		| Random =>
			let
				fun length' index () = index + 1
				fun sub' index revIndex =
					if revIndex < 0 then raise Subscript
					else sub (t, index - revIndex)
				fun getItem' index () =
					if index < 0 then NONE
					else SOME (
						sub (t, index)
						, create' (index - 1)
					)
				and create' index = create {
					access = Random
					, length = length' index
					, sub = sub' index
					, getItem = getItem' index
				}
			in
				create' (length t - 1)
			end
	fun fromRevShadow t = rev t
	fun toShadow t = t
	fun revToShadow t = rev t
	fun revToList t =
		let
			fun loop (t, l) = case getItem t of
				NONE => l
				| SOME (x, t) => loop (t, x :: l)
		in
			loop (t, nil)
		end
	fun toList t = case access t of
		Random => revToList (rev t)
		| Sequential => (case getItem t of
			NONE => nil
			| SOME (x, t) => x :: toList t
		)
	fun revToTabulated tabulate t = case access t of
		Sequential => toTabulated tabulate (rev t)
		| Random =>
			let
				val n = length t
			in
				tabulate (n, fn i => sub (t, n - 1 - i))
			end
	fun revToArray t = revToTabulated Array.tabulate t
	val revToBoolArray = revToTabulated BoolArray.tabulate
	val revToCharArray = revToTabulated CharArray.tabulate
	val revToIntArray = revToTabulated IntArray.tabulate
	val revToWordArray = revToTabulated WordArray.tabulate
	val revToRealArray = revToTabulated RealArray.tabulate
	val revToLargeIntArray = revToTabulated LargeIntArray.tabulate
	val revToLargeWordArray = revToTabulated LargeWordArray.tabulate
	val revToInt8Array = revToTabulated Int8Array.tabulate
	val revToInt16Array = revToTabulated Int16Array.tabulate
	val revToInt32Array = revToTabulated Int32Array.tabulate
	val revToInt64Array = revToTabulated Int64Array.tabulate
	val revToWord8Array = revToTabulated Word8Array.tabulate
	val revToWord16Array = revToTabulated Word16Array.tabulate
	val revToWord32Array = revToTabulated Word32Array.tabulate
	val revToWord64Array = revToTabulated Word64Array.tabulate
	val revToReal32Array = revToTabulated Real32Array.tabulate
	val revToReal64Array = revToTabulated Real64Array.tabulate
	fun revToVector t = revToTabulated Vector.tabulate t
	val revToBoolVector = revToTabulated BoolVector.tabulate
	val revToCharVector = revToTabulated CharVector.tabulate
	val revToIntVector = revToTabulated IntVector.tabulate
	val revToWordVector = revToTabulated WordVector.tabulate
	val revToRealVector = revToTabulated RealVector.tabulate
	val revToLargeIntVector = revToTabulated LargeIntVector.tabulate
	val revToLargeWordVector = revToTabulated LargeWordVector.tabulate
	val revToInt8Vector = revToTabulated Int8Vector.tabulate
	val revToInt16Vector = revToTabulated Int16Vector.tabulate
	val revToInt32Vector = revToTabulated Int32Vector.tabulate
	val revToInt64Vector = revToTabulated Int64Vector.tabulate
	val revToWord8Vector = revToTabulated Word8Vector.tabulate
	val revToWord16Vector = revToTabulated Word16Vector.tabulate
	val revToWord32Vector = revToTabulated Word32Vector.tabulate
	val revToWord64Vector = revToTabulated Word64Vector.tabulate
	val revToReal32Vector = revToTabulated Real32Vector.tabulate
	val revToReal64Vector = revToTabulated Real64Vector.tabulate
	val revToString = revToCharVector
	fun append (a, b) = create {
		access = case (access a, access b) of
			(Random, Random) => Random
			| _ => Sequential
		, length = fn () => length a + length b
		, getItem = fn () => (case getItem a of
			NONE => getItem b
			| SOME (x, a) => SOME (
				x
				, append (a, b)
			)
		), sub = fn i =>
			let
				val n = length a
			in
				if i < n then sub (a, i)
				else sub (b, i - n)
			end
	}
	fun foldl f b t = case getItem t of
		NONE => b
		| SOME (a, t) => foldl f (f (a, b)) t
	val fold = foldl
	fun foldli f b t =
		let
			fun loop (i, b, t) = case getItem t of
				NONE => b
				| SOME (a, t) => loop (i + 1, f (i, a, b), t)
		in
			loop (0, b, t)
		end
	val foldi = foldli
	fun foldr f b t = foldl f b (rev t)
	fun foldri f b t = foldli f b (rev t)
	fun reducel f t =
		let
			fun loop (a, t) = case getItem t of
				NONE => a
				| SOME (b, t) => loop (f (b, a), t)
		in
			case getItem t of
				NONE => raise Empty
				| SOME (a, t) => loop (a, t)
		end
	val reduce = reducel
	fun reducer f t = reducel f (rev t)
	fun map f t = create {
		access = access t
		, length = fn () => length t
		, getItem = fn () => (case getItem t of
			NONE => NONE
			| SOME (x, t) => SOME (
				f x
				, map f t
			)
		), sub = fn i => f (sub (t, i))
	}
	fun mapi f t =
		let
			fun sub' (i, t) j = f (i + j, sub (t, j))
			fun getItem' (i, t) () = case getItem t of
				NONE => NONE
				| SOME (x, t) => SOME (
					f (i, x)
					, create' (i + 1, t)
				)
			and create' (i, t) = create {
				access = access t
				, length = fn () => length t
				, getItem = getItem' (i, t)
				, sub = sub' (i, t)
			}
		in
			create' (0, t)
		end
	fun appl f t = case getItem t of
		NONE => ()
		| SOME (x, t) => (
			f x
			; appl f t
		)
	val app = appl
	fun appr f t = appl f (rev t)
	fun appli f t =
		let
			fun loop (i, t) = case getItem t of
				NONE => ()
				| SOME (x, t) => (
					f (i, x)
					; loop (i + 1, t)
				)
		in
			loop (0, t)
		end
	val appi = appli
	fun appri f t = appl f (rev (mapi (fn x => x) t))
	fun filter f t =
		let
			fun next' t = case getItem t of
				NONE => NONE
				| SOME (x, t) =>
					if f x then SOME (
						x
						, filter f t
					) else next' t
		in
			fromFunction (fn () => next' t)
		end
	fun mapPartial f t =
		let
			fun next' t () = case getItem t of
				NONE => NONE
				| SOME (x, t) => (case f x of
					NONE => next' t ()
					| SOME x => SOME (
						x
						, mapPartial f t
					)
				)
		in
			fromFunction (next' t)
		end
	fun zip (a, b) = create {
		access = case (access a, access b) of
			(Random, Random) => Random
			| _ => Sequential
		, length = fn () => Int.min (length a, length b)
		, getItem = fn () => (case (getItem a, getItem b) of
			(SOME (xA, tA), SOME (xB, tB)) => SOME (
				(xA, xB)
				, zip (tA, tB)
			) | _ => NONE
		), sub = fn i => (sub (a, i), sub (b, i))
	}
	fun unzip t = (
		map (fn (x, _) => x) t
		, map (fn (_, y) => y) t
	)
	fun ungetItem (t, x) = create {
		access = access t
		, length = fn () => length t + 1
		, getItem = fn () => SOME (x, t)
		, sub = fn
			0 => x
			| i => sub (t, i - 1)
	}
	fun concat t =
		let
			fun length' () = fold (op +) 0 (map length t)
			fun getItem' t () = case getItem t of
				NONE => NONE
				| SOME (u, v) => (case getItem u of
					NONE => getItem' v ()
					| SOME (x, w) => SOME (
						x
						, concat (ungetItem (v, w))
					)
				)
			fun sub' t i =
				let
					fun loop (t, offset) = case getItem t of
						NONE => raise Subscript
						| SOME (u, v) =>
							let
								val n = length u
							in
								if n + offset > i then
									sub (u, i - offset)
								else loop (v, offset + n)
							end
				in
					loop (t, 0)
				end
		in
			create {
				access = Sequential
				, length = length'
				, getItem = getItem' t
				, sub = sub' t
			}
		end
	fun concatWith s t =
		let
			fun length' t () = Int.max (0, length t * 2 - 1)
			fun isOdd n = Word.andb (Word.fromInt n, 0w1) = 0w1
			fun sub' t i =
				if i >= length' t () then raise Subscript
				else if isOdd i then s
				else sub (t, i div 2)
			fun getItem' t () = case getItem t of
				NONE => NONE
				| x as (SOME (y, u)) => (case getItem u of
					NONE => x
					| SOME _ => SOME (
						y
						, ungetItem (create' u, s)
					)
				)
			and create' t = create {
				access = Sequential
				, length = length' t
				, getItem = getItem' t
				, sub = sub' t
			}
		in
			concat (create' t)
		end
	fun pairs t = create {
		access = access t
		, length = fn () => length t div 2
		, getItem = fn () => (case getItem t of
			NONE => NONE
			| SOME (a, u) => (case getItem u of
				NONE => NONE
				| SOME (b, v) => SOME ((a, b), pairs v)
			)
		), sub = fn i =>
			let
				val j = i * 2
			in
				(sub (t, j), sub (t, j + 1))
			end
	}
	fun findl f t = case getItem t of
		NONE => NONE
		| SOME (x, t) =>
			if f x then SOME x
			else findl f t
	val find = findl
	fun findr f t = findl f (rev t)
	fun findli f t =
		let
			fun loop (i, t) = case getItem t of
				NONE => NONE
				| SOME (x, t) =>
					if f (i, x) then SOME (i, x)
					else loop (i + 1, t)
		in
			loop (0, t)
		end
	val findi = findli
	fun findri f t = findli f (rev t)
	fun partition f t = (
		filter f t
		, filter (not o f) t
	)
	fun exists f t = case getItem t of
		NONE => false
		| SOME (x, t) => f x orelse exists f t
	fun all f t = case getItem t of
		NONE => true
		| SOME (x, t) => f x andalso all f t
	fun tabulate (n, f) = fromRandom {
		length = fn () => n
		, sub = f
	}
	fun collate f (a, b) = case (getItem a, getItem b) of
		(NONE, NONE) => EQUAL
		| (NONE, SOME _) => LESS
		| (SOME _, NONE) => GREATER
		| (SOME (xA, tA), SOME (xB, tB)) => (case f (xA, xB) of
			EQUAL => collate f (tA, tB)
			| y => y
		)
	fun drop f t = case getItem t of
		NONE => t
		| SOME (x, u) =>
			if f x then drop f u
			else u
	fun take f t =
		let
			fun next' () = case getItem t of
				NONE => NONE
				| SOME (x, t) =>
					if f x then SOME (x, take f t)
					else NONE
		in
			fromIdempotentFunction next'
		end
	fun split f t = (
		take f t
		, drop f t
	)
	fun trim (t, n) = case access t of
		Random => fromRandom {
			length = fn () => length t - n
			, sub = fn i => sub (t, i + n)
		} | Sequential =>
			let
				fun loop (t, n) =
					if n > 0 then case getItem t of
						NONE => t
						| SOME (x, u) => loop (u, n - 1)
					else t
			in
				loop (t, n)
			end
	fun limit (t, n) =
		let
			fun sub' n i =
				if i < n then sub (t, i)
				else raise Subscript
			fun length' n () = n
			fun getItem' n () =
				if n > 0 then case getItem t of
					NONE => NONE
					| SOME (x, u) => SOME (
						x
						, limit (u, n - 1)
					)
				else NONE
			fun create' n = create {
				access = access t
				, length = length' n
				, getItem = getItem' n
				, sub = sub' n
			}
		in
			create' n
		end
	fun group (t, n) = create {
		access = access t
		, length = fn () => (length t + 1) div n
		, getItem = fn () => (case getItem t of
			NONE => NONE
			| SOME _ => SOME (
				limit (t, n)
				, group (trim (t, n), n)
			)
		), sub = fn i => limit (trim (t, n * i), n)
	}
	fun splitAt (t, i) = (
		limit (t, i)
		, trim (t, i)
	)
	fun tokens (f: 'a -> bool) (t: 'a t): 'a t t =
		let
			val t = drop f t
			fun next' () = case getItem t of
				NONE => NONE
				| SOME _ => SOME (
					take (not o f) t
					, tokens f (drop (not o f) t)
				)
		in
			fromFunction next'
		end
	fun fields f t =
		let
			fun next' () = case getItem t of
				NONE => NONE
				| SOME _ => SOME (
					take (not o f) t
					, fields f (
						let
							val t = drop (not o f) t
						in
							case getItem t of
								NONE => t
								| SOME (_, t) => t
						end
					)
				)
		in
			fromFunction next'
		end
	fun translate f t = concat (map f t)
	fun sub (t, i) = case getItem t of
		NONE => raise Subscript
		| SOME (x, t) =>
			if i = 0 then x
			else sub (t, i - 1)
	fun update (t, i, x) =
		let
			val prefix = limit (t, i)
			val suffix = trim (t, i + 1)
		in
			append (
				prefix
				, append (
					single x
					, suffix
				)
			)
		end
	fun delete (t, i) =
		let
			val prefix = limit (t, i)
			val suffix = trim (t, i + 1)
		in
			append (prefix, suffix)
		end
	fun insert (t, i, x) = 
		let
			val prefix = limit (t, i)
			val suffix = trim (t, i)
		in
			append (
				prefix
				, append (
					single x
					, suffix
				)
			)
		end
	fun extract (t, i, n) = case n of
		NONE => trim (t, i)
		| SOME n => limit (trim (t, i), n)
	fun isEmpty t = case getItem t of NONE => true | SOME _ => false
	fun copyShadow {src, dst, di} =
		if length src + di > length dst then raise Subscript
		else append (
			extract (dst, 0, SOME di)
			, append (
				src
				, extract (dst, di + length src, NONE)
			)
		)
end
