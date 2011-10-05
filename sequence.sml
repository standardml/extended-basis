(*
	Sequence
	Abstract Signatures for Integer-Addressable Polymorphic Collections
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
signature SEQUENCE = sig
	type 'a sequence

	(*
		isEmpty t returns true if t is empty, and false if
		t is not empty.
	*)
	val isEmpty: 'a sequence -> bool

	(*
		length t returns the number of elements in t.
		If t does not have a finite length, then it raises
		InfiniteLength.
	*)
	val length: 'a sequence -> int

	(*
		hd t returns the element at the head of the sequence.
		It raises Empty if t is empty.
	*)
	val hd: 'a sequence -> 'a

	(*
		sub (t, i) returns the ith element of t. It
		raises Subscript if the end of the sequence is
		reached before the ith element.
	*)
	val sub: 'a sequence * int -> 'a

	(*
		appl f t applies f to every element of t, from
		the first to the last element.
		app is equivalent to appl.
	*)
	val appl: ('a -> unit) -> 'a sequence -> unit
	val app: ('a -> unit) -> 'a sequence -> unit

	(*
		appli f t applies f to a pair of the index of each
		element of t, along with the element itself, from the
		first to the last.
		appi is equivalent to appli.
	*)
	val appli: (int * 'a -> unit) -> 'a sequence -> unit
	val appi: (int * 'a -> unit) -> 'a sequence -> unit

	(*
		appr f t applies f to every element of t, from
		the last to the first element.
	*)
	val appr: ('a -> unit) -> 'a sequence -> unit

	(*
		appri f t applies f to a pair of the index of each
		element of t, along with the element itself, from the
		last to the first.
	*)
	val appri: (int * 'a -> unit) -> 'a sequence -> unit

	(*
		foldl f b t returns f (xn, ... f (x1, f (x0, b)) ...),
		where x0, x1, ... xn are the elements of t, or simply b
		if the sequence is empty.
		fold is equivalent to foldl.
	*)
	val foldl: ('a * 'b -> 'b) -> 'b -> 'a sequence -> 'b
	val fold: ('a * 'b -> 'b) -> 'b -> 'a sequence -> 'b

	(*
		foldli f b t returns (f (n, xn, ... f (1, x1, f (0, x0, b)) ...),
		where x0, x1, ... xn are the elements of t, or simply b
		if the sequence is empty.
		foldi is equivalent to foldli.
	*)
	val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a sequence -> 'b
	val foldi: (int * 'a * 'b -> 'b) -> 'b -> 'a sequence -> 'b

	val foldr: ('a * 'b -> 'b) -> 'b -> 'a sequence -> 'b
	val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a sequence -> 'b

	(*
		reducel f t returns f (xn, ... f (x2, f (x1, x0)) ...),
		where x0, x1, x2, ... xn are the elements of t, or if t
		contains a single element, simply returns that element.
		If t is empty, it raises Empty.
		reduce is equivalent to reducel.
	*)
	val reducel: ('a * 'a -> 'a) -> 'a sequence -> 'a
	val reduce: ('a * 'a -> 'a) -> 'a sequence -> 'a

	(*
		reducer f t returns f (x0, f (x1, ... f (xn-1, xn) ...)),
		where x0, x1, ... xn-1, xn are the elements of t, of if t
		contains a single element, simply returns that element.
		If t is empty it raises Empty.
	*)
	val reducer: ('a * 'a -> 'a) -> 'a sequence -> 'a

	(*
		findl f t returns SOME x, in which x is the
		first element which, applied to f, results in true.
		If no element results in true, it returns NONE.
		find is equivalent to findl.
	*)
	val findl: ('a -> bool) -> 'a sequence -> 'a option
	val find: ('a -> bool) -> 'a sequence -> 'a option

	(*
		findli f t returns SOME (i, x), in which i is the
		index of the first element which, applied to f, results
		in true, and x is the element itself. If no element
		results in true, it returns NONE.
		findi is equivalent to findli.
	*)
	val findli: (int * 'a -> bool) -> 'a sequence -> (int * 'a) option
	val findi: (int * 'a -> bool) -> 'a sequence -> (int * 'a) option

	(*
		findr f t returns SOME x, in which x is the
		last element which, applied to f, results in true.
		If no element results in true, it returns NONE.
	*)
	val findr: ('a -> bool) -> 'a sequence -> 'a option

	(*
		findri f t returns SOME (i, x), in which i is the
		index of the last element which, applied to f, results
		in true, and x is the element itself. If no element
		results in true, it returns NONE.
	*)
	val findri: (int * 'a -> bool) -> 'a sequence -> (int * 'a) option

	(*
		exists f t returns true on the first element which, when
		applied to f, yields true, and returns false if no such
		element is found.
	*)
	val exists: ('a -> bool) -> 'a sequence -> bool

	(*
		all f t returns false on the first element which, when
		applied to f, yields false, and returns true if no such
		element is found.
	*)
	val all: ('a -> bool) -> 'a sequence -> bool

	(*
		collate f (t, u) compares t and u lexicographically,
		using f to compare elements of each, and returns LESS if
		t is less than u, EQUAL if t is equal to u, and GREATER
		if t is greater than u.
	*)
	val collate: ('a * 'a -> order) -> 'a sequence * 'a sequence -> order

	(*
		Convert to another sequence type. The functions with rev
		reverse before conversion.
	*)
	val toShadow: 'a sequence -> 'a Shadow.t
	val revToShadow: 'a sequence -> 'a Shadow.t
	val toList: 'a sequence -> 'a list
	val revToList: 'a sequence -> 'a list
	val toArray: 'a sequence -> 'a array
	val revToArray: 'a sequence -> 'a array
	val toCharArray: char sequence -> CharArray.array
	val revToCharArray: char sequence -> CharArray.array
	val toBoolArray: bool sequence -> BoolArray.array
	val revToBoolArray: bool sequence -> BoolArray.array
	val toIntArray: int sequence -> IntArray.array
	val revToIntArray: int sequence -> IntArray.array
	val toWordArray: word sequence -> WordArray.array
	val revToWordArray: word sequence -> WordArray.array
	val toRealArray: real sequence -> RealArray.array
	val revToRealArray: real sequence -> RealArray.array
	val toLargeIntArray: LargeInt.int sequence -> LargeIntArray.array
	val revToLargeIntArray: LargeInt.int sequence -> LargeIntArray.array
	val toLargeWordArray: LargeWord.word sequence -> LargeWordArray.array
	val revToLargeWordArray: LargeWord.word sequence -> LargeWordArray.array
	val toInt8Array: Int8.int sequence -> Int8Array.array
	val revToInt8Array: Int8.int sequence -> Int8Array.array
	val toInt16Array: Int16.int sequence -> Int16Array.array
	val revToInt16Array: Int16.int sequence -> Int16Array.array
	val toInt32Array: Int32.int sequence -> Int32Array.array
	val revToInt32Array: Int32.int sequence -> Int32Array.array
	val toInt64Array: Int64.int sequence -> Int64Array.array
	val revToInt64Array: Int64.int sequence -> Int64Array.array
	val toWord8Array: Word8.word sequence -> Word8Array.array
	val revToWord8Array: Word8.word sequence -> Word8Array.array
	val toWord16Array: Word16.word sequence -> Word16Array.array
	val revToWord16Array: Word16.word sequence -> Word16Array.array
	val toWord32Array: Word32.word sequence -> Word32Array.array
	val revToWord32Array: Word32.word sequence -> Word32Array.array
	val toWord64Array: Word64.word sequence -> Word64Array.array
	val revToWord64Array: Word64.word sequence -> Word64Array.array
	val toReal32Array: Real32.real sequence -> Real32Array.array
	val revToReal32Array: Real32.real sequence -> Real32Array.array
	val toReal64Array: Real64.real sequence -> Real64Array.array
	val revToReal64Array: Real64.real sequence -> Real64Array.array
	val toVector: 'a sequence -> 'a vector
	val revToVector: 'a sequence -> 'a vector
	val toCharVector: char sequence -> CharVector.vector
	val revToCharVector: char sequence -> CharVector.vector
	val toBoolVector: bool sequence -> BoolVector.vector
	val revToBoolVector: bool sequence -> BoolVector.vector
	val toIntVector: int sequence -> IntVector.vector
	val revToIntVector: int sequence -> IntVector.vector
	val toWordVector: word sequence -> WordVector.vector
	val revToWordVector: word sequence -> WordVector.vector
	val toRealVector: real sequence -> RealVector.vector
	val revToRealVector: real sequence -> RealVector.vector
	val toLargeIntVector: LargeInt.int sequence -> LargeIntVector.vector
	val revToLargeIntVector: LargeInt.int sequence -> LargeIntVector.vector
	val toLargeWordVector: LargeWord.word sequence -> LargeWordVector.vector
	val revToLargeWordVector: LargeWord.word sequence -> LargeWordVector.vector
	val toInt8Vector: Int8.int sequence -> Int8Vector.vector
	val revToInt8Vector: Int8.int sequence -> Int8Vector.vector
	val toInt16Vector: Int16.int sequence -> Int16Vector.vector
	val revToInt16Vector: Int16.int sequence -> Int16Vector.vector
	val toInt32Vector: Int32.int sequence -> Int32Vector.vector
	val revToInt32Vector: Int32.int sequence -> Int32Vector.vector
	val toInt64Vector: Int64.int sequence -> Int64Vector.vector
	val revToInt64Vector: Int64.int sequence -> Int64Vector.vector
	val toWord8Vector: Word8.word sequence -> Word8Vector.vector
	val revToWord8Vector: Word8.word sequence -> Word8Vector.vector
	val toWord16Vector: Word16.word sequence -> Word16Vector.vector
	val revToWord16Vector: Word16.word sequence -> Word16Vector.vector
	val toWord32Vector: Word32.word sequence -> Word32Vector.vector
	val revToWord32Vector: Word32.word sequence -> Word32Vector.vector
	val toWord64Vector: Word64.word sequence -> Word64Vector.vector
	val revToWord64Vector: Word64.word sequence -> Word64Vector.vector
	val toReal32Vector: Real32.real sequence -> Real32Vector.vector
	val revToReal32Vector: Real32.real sequence -> Real32Vector.vector
	val toReal64Vector: Real64.real sequence -> Real64Vector.vector
	val revToReal64Vector: Real64.real sequence -> Real64Vector.vector
	val toString: char sequence -> string
	val revToString: char sequence -> string
end

signature CREATEABLE_SEQUENCE = sig
	include SEQUENCE
	val maxLen: int

	(*
		empty () returns an empty sequence.
	*)
	val empty: unit -> 'a sequence

	(*
		single x returns a sequenge which contains a single
		element of x.
	*)
	val single: 'a -> 'a sequence

	(*
		unfold f a0 creates a sequence b0, b1, ... bn, through
		a process where f a0 yields SOME (b0, a1), f a1 yields
		SOME (b1, a2), etc. until f an yields SOME (bn, an+1)
		and f an+1 yields NONE.
	*)
	val unfold: ('a -> ('b * 'a) option) -> 'a -> 'b sequence

	(*
		unfoldn f (n, a0) creates a sequence b0, b1, ... bn,
		through a process where f a0 yields (b0, a1), f a1
		yields (b1, a2), etc. until the sequence reaches length
		n.
	*)
	val unfoldn: ('a -> 'b * 'a) -> int * 'a -> 'b sequence

	(*
		tabulate (n, f) creates a sequence of length n, where
		the elements are f 0, f 1, ... f (n - 1).
	*)
	val tabulate: int * (int -> 'a) -> 'a sequence

	(*
		split f t returns two subsequences: the first consists
		all the elements from at the beginning of t
		that satisfy the predicate f, and the second consists of
		the remaining elements.
	*)
	val split: ('a -> bool) -> 'a sequence -> 'a sequence * 'a sequence

	(*
		splitAt (t, n) returns two subsequences: the first consists
		of the first n elements of t, and the second consists of
		the remaining elements.
	*)
	val splitAt: 'a sequence * int -> 'a sequence * 'a sequence

	(*
		drop f t returns the sequence remaining after dropping the
		elements from the beginning of t that, when applied
		to f, return true.
	*)
	val drop: ('a -> bool) -> 'a sequence -> 'a sequence

	(*
		trim (t, n) returns the sequence remaining after trimming
		n elements from the beginning of t.
	*)
	val trim: 'a sequence * int -> 'a sequence

	(*
		limit (t, n) returns a sequence limited to the first
		n elements of t.
	*)
	val limit: 'a sequence * int -> 'a sequence

	(*
		take f t returns a sequence consisting of the elements
		from the beginning of t that, when applied to f, yield
		true.
	*)
	val take: ('a -> bool) -> 'a sequence -> 'a sequence

	(*
		tokens f t returns a sequence of subsequences separated
		by elements that, when applied to f, yield true. Multiple
		adjacent separating elements are treated as a single separator,
		thus there are no empty subsequences in the result.
	*)
	val tokens: ('a -> bool) -> 'a sequence -> 'a sequence sequence

	(*
		fields f t returns a sequence of subsequences separated
		by elements that, when applied to f, yield true. Multiple
		adjacent separating elements are treated as separating
		empty subsequences, contrary to tokens above.
	*)
	val fields: ('a -> bool) -> 'a sequence -> 'a sequence sequence

	(*
		translate f t returns a sequence of the concatenation
		of the results of f applied to each element of t.
	*)
	val translate: ('a -> 'a sequence) -> 'a sequence -> 'a sequence

	(*
		extract (t, i, NONE) returns a subsequence of t,
		starting at the ith element and continuing to the end.
		extract (t, i, SOME n) returns a subsequence of t,
		starting at the ith element and continuing for n elements.
	*)
	val extract: 'a sequence * int * int option -> 'a sequence

	(*
		insert (t, i, x) returns a new sequence, with
		x inserted at position i. The elements from
		position i to the end of the sequence are
		shifted to the right.
	*)
	val insert: 'a sequence * int * 'a -> 'a sequence

	(*
		delete (t, i) returns a new sequence, with the
		the ith element in t removed.
	*)
	val delete: 'a sequence * int -> 'a sequence

	(*
		These convert from another data structure to this
		type of sequence. The ones with Rev in the name
		reverse while converting.
	*)
	val fromShadow: 'a Shadow.t -> 'a sequence
	val fromRevShadow: 'a Shadow.t -> 'a sequence
	val fromArray: 'a array -> 'a sequence
	val fromRevArray: 'a array -> 'a sequence
	val fromCharArray: CharArray.array -> char sequence
	val fromRevCharArray: CharArray.array -> char sequence
	val fromBoolArray: BoolArray.array -> bool sequence
	val fromRevBoolArray: BoolArray.array -> bool sequence
	val fromIntArray: IntArray.array -> int sequence
	val fromRevIntArray: IntArray.array -> int sequence
	val fromWordArray: WordArray.array -> word sequence
	val fromRevWordArray: WordArray.array -> word sequence
	val fromRealArray: RealArray.array -> real sequence
	val fromRevRealArray: RealArray.array -> real sequence
	val fromLargeIntArray: LargeIntArray.array -> LargeInt.int sequence
	val fromRevLargeIntArray: LargeIntArray.array -> LargeInt.int sequence
	val fromLargeWordArray: LargeWordArray.array -> LargeWord.word sequence
	val fromRevLargeWordArray: LargeWordArray.array -> LargeWord.word sequence
	val fromInt8Array: Int8Array.array -> Int8.int sequence
	val fromRevInt8Array: Int8Array.array -> Int8.int sequence
	val fromInt16Array: Int16Array.array -> Int16.int sequence
	val fromRevInt16Array: Int16Array.array -> Int16.int sequence
	val fromInt32Array: Int32Array.array -> Int32.int sequence
	val fromRevInt32Array: Int32Array.array -> Int32.int sequence
	val fromInt64Array: Int64Array.array -> Int64.int sequence
	val fromRevInt64Array: Int64Array.array -> Int64.int sequence
	val fromWord8Array: Word8Array.array -> Word8.word sequence
	val fromRevWord8Array: Word8Array.array -> Word8.word sequence
	val fromWord16Array: Word16Array.array -> Word16.word sequence
	val fromRevWord16Array: Word16Array.array -> Word16.word sequence
	val fromWord32Array: Word32Array.array -> Word32.word sequence
	val fromRevWord32Array: Word32Array.array -> Word32.word sequence
	val fromWord64Array: Word64Array.array -> Word64.word sequence
	val fromRevWord64Array: Word64Array.array -> Word64.word sequence
	val fromReal32Array: Real32Array.array -> Real32.real sequence
	val fromRevReal32Array: Real32Array.array -> Real32.real sequence
	val fromReal64Array: Real64Array.array -> Real64.real sequence
	val fromRevReal64Array: Real64Array.array -> Real64.real sequence
	val fromArraySlice: 'a ArraySlice.slice -> 'a sequence
	val fromRevArraySlice: 'a ArraySlice.slice -> 'a sequence
	val fromCharArraySlice: CharArraySlice.slice -> char sequence
	val fromRevCharArraySlice: CharArraySlice.slice -> char sequence
	val fromBoolArraySlice: BoolArraySlice.slice -> bool sequence
	val fromRevBoolArraySlice: BoolArraySlice.slice -> bool sequence
	val fromIntArraySlice: IntArraySlice.slice -> int sequence
	val fromRevIntArraySlice: IntArraySlice.slice -> int sequence
	val fromWordArraySlice: WordArraySlice.slice -> word sequence
	val fromRevWordArraySlice: WordArraySlice.slice -> word sequence
	val fromRealArraySlice: RealArraySlice.slice -> real sequence
	val fromRevRealArraySlice: RealArraySlice.slice -> real sequence
	val fromLargeIntArraySlice: LargeIntArraySlice.slice -> LargeInt.int sequence
	val fromRevLargeIntArraySlice: LargeIntArraySlice.slice -> LargeInt.int sequence
	val fromLargeWordArraySlice: LargeWordArraySlice.slice -> LargeWord.word sequence
	val fromRevLargeWordArraySlice: LargeWordArraySlice.slice -> LargeWord.word sequence
	val fromInt8ArraySlice: Int8ArraySlice.slice -> Int8.int sequence
	val fromRevInt8ArraySlice: Int8ArraySlice.slice -> Int8.int sequence
	val fromInt16ArraySlice: Int16ArraySlice.slice -> Int16.int sequence
	val fromRevInt16ArraySlice: Int16ArraySlice.slice -> Int16.int sequence
	val fromInt32ArraySlice: Int32ArraySlice.slice -> Int32.int sequence
	val fromRevInt32ArraySlice: Int32ArraySlice.slice -> Int32.int sequence
	val fromInt64ArraySlice: Int64ArraySlice.slice -> Int64.int sequence
	val fromRevInt64ArraySlice: Int64ArraySlice.slice -> Int64.int sequence
	val fromWord8ArraySlice: Word8ArraySlice.slice -> Word8.word sequence
	val fromRevWord8ArraySlice: Word8ArraySlice.slice -> Word8.word sequence
	val fromWord16ArraySlice: Word16ArraySlice.slice -> Word16.word sequence
	val fromRevWord16ArraySlice: Word16ArraySlice.slice -> Word16.word sequence
	val fromWord32ArraySlice: Word32ArraySlice.slice -> Word32.word sequence
	val fromRevWord32ArraySlice: Word32ArraySlice.slice -> Word32.word sequence
	val fromWord64ArraySlice: Word64ArraySlice.slice -> Word64.word sequence
	val fromRevWord64ArraySlice: Word64ArraySlice.slice -> Word64.word sequence
	val fromVector: 'a vector -> 'a sequence
	val fromRevVector: 'a vector -> 'a sequence
	val fromCharVector: CharVector.vector -> char sequence
	val fromRevCharVector: CharVector.vector -> char sequence
	val fromBoolVector: BoolVector.vector -> bool sequence
	val fromRevBoolVector: BoolVector.vector -> bool sequence
	val fromIntVector: IntVector.vector -> int sequence
	val fromRevIntVector: IntVector.vector -> int sequence
	val fromWordVector: WordVector.vector -> word sequence
	val fromRevWordVector: WordVector.vector -> word sequence
	val fromRealVector: RealVector.vector -> real sequence
	val fromRevRealVector: RealVector.vector -> real sequence
	val fromLargeIntVector: LargeIntVector.vector -> LargeInt.int sequence
	val fromRevLargeIntVector: LargeIntVector.vector -> LargeInt.int sequence
	val fromLargeWordVector: LargeWordVector.vector -> LargeWord.word sequence
	val fromRevLargeWordVector: LargeWordVector.vector -> LargeWord.word sequence
	val fromInt8Vector: Int8Vector.vector -> Int8.int sequence
	val fromRevInt8Vector: Int8Vector.vector -> Int8.int sequence
	val fromInt16Vector: Int16Vector.vector -> Int16.int sequence
	val fromRevInt16Vector: Int16Vector.vector -> Int16.int sequence
	val fromInt32Vector: Int32Vector.vector -> Int32.int sequence
	val fromRevInt32Vector: Int32Vector.vector -> Int32.int sequence
	val fromInt64Vector: Int64Vector.vector -> Int64.int sequence
	val fromRevInt64Vector: Int64Vector.vector -> Int64.int sequence
	val fromWord8Vector: Word8Vector.vector -> Word8.word sequence
	val fromRevWord8Vector: Word8Vector.vector -> Word8.word sequence
	val fromWord16Vector: Word16Vector.vector -> Word16.word sequence
	val fromRevWord16Vector: Word16Vector.vector -> Word16.word sequence
	val fromWord32Vector: Word32Vector.vector -> Word32.word sequence
	val fromRevWord32Vector: Word32Vector.vector -> Word32.word sequence
	val fromWord64Vector: Word64Vector.vector -> Word64.word sequence
	val fromRevWord64Vector: Word64Vector.vector -> Word64.word sequence
	val fromReal32Vector: Real32Vector.vector -> Real32.real sequence
	val fromRevReal32Vector: Real32Vector.vector -> Real32.real sequence
	val fromReal64Vector: Real64Vector.vector -> Real64.real sequence
	val fromRevReal64Vector: Real64Vector.vector -> Real64.real sequence
	val fromVectorSlice: 'a VectorSlice.slice -> 'a sequence
	val fromRevVectorSlice: 'a VectorSlice.slice -> 'a sequence
	val fromCharVectorSlice: CharVectorSlice.slice -> char sequence
	val fromRevCharVectorSlice: CharVectorSlice.slice -> char sequence
	val fromBoolVectorSlice: BoolVectorSlice.slice -> bool sequence
	val fromRevBoolVectorSlice: BoolVectorSlice.slice -> bool sequence
	val fromIntVectorSlice: IntVectorSlice.slice -> int sequence
	val fromRevIntVectorSlice: IntVectorSlice.slice -> int sequence
	val fromWordVectorSlice: WordVectorSlice.slice -> word sequence
	val fromRevWordVectorSlice: WordVectorSlice.slice -> word sequence
	val fromRealVectorSlice: RealVectorSlice.slice -> real sequence
	val fromRevRealVectorSlice: RealVectorSlice.slice -> real sequence
	val fromLargeIntVectorSlice: LargeIntVectorSlice.slice -> LargeInt.int sequence
	val fromRevLargeIntVectorSlice: LargeIntVectorSlice.slice -> LargeInt.int sequence
	val fromLargeWordVectorSlice: LargeWordVectorSlice.slice -> LargeWord.word sequence
	val fromRevLargeWordVectorSlice: LargeWordVectorSlice.slice -> LargeWord.word sequence
	val fromInt8VectorSlice: Int8VectorSlice.slice -> Int8.int sequence
	val fromRevInt8VectorSlice: Int8VectorSlice.slice -> Int8.int sequence
	val fromInt16VectorSlice: Int16VectorSlice.slice -> Int16.int sequence
	val fromRevInt16VectorSlice: Int16VectorSlice.slice -> Int16.int sequence
	val fromInt32VectorSlice: Int32VectorSlice.slice -> Int32.int sequence
	val fromRevInt32VectorSlice: Int32VectorSlice.slice -> Int32.int sequence
	val fromInt64VectorSlice: Int64VectorSlice.slice -> Int64.int sequence
	val fromRevInt64VectorSlice: Int64VectorSlice.slice -> Int64.int sequence
	val fromWord8VectorSlice: Word8VectorSlice.slice -> Word8.word sequence
	val fromRevWord8VectorSlice: Word8VectorSlice.slice -> Word8.word sequence
	val fromWord16VectorSlice: Word16VectorSlice.slice -> Word16.word sequence
	val fromRevWord16VectorSlice: Word16VectorSlice.slice -> Word16.word sequence
	val fromWord32VectorSlice: Word32VectorSlice.slice -> Word32.word sequence
	val fromRevWord32VectorSlice: Word32VectorSlice.slice -> Word32.word sequence
	val fromWord64VectorSlice: Word64VectorSlice.slice -> Word64.word sequence
	val fromRevWord64VectorSlice: Word64VectorSlice.slice -> Word64.word sequence
	val fromString: string -> char sequence
	val fromRevString: string -> char sequence
	val fromSubstring: Substring.substring -> char sequence
	val fromRevSubstring: Substring.substring -> char sequence
	val fromList: 'a list -> 'a sequence
	val fromTextIO: TextIO.instream -> char sequence
	val fromTextIOLines: TextIO.instream -> string sequence
	val fromBinIO: BinIO.instream -> Word8.word sequence

	(*
		map f t returns a sequence of the results of applying f to
		each element of t.
	*)
	val map: ('a -> 'b) -> 'a sequence -> 'b sequence

	(*
		mapi f t returns a sequence of the results of applying f
		to a pair of the index of each element of t, along with the element
		itself.
	*)
	val mapi: (int * 'a -> 'b) -> 'a sequence -> 'b sequence

	(*
		getItem t returns NONE if t is empty, or SOME (x, u)
		in which x is the next element and u is a sequence
		containing the remaining elements.
	*)
	val getItem: 'a sequence -> ('a * 'a sequence) option

	(*
		ungetItem (t, x) returns a sequence in which x is
		the first element, and t makes up the remaining
		elements.
	*)
	val ungetItem: 'a sequence * 'a -> 'a sequence

	(*
		tl t returns the sequence consisting of all but the
		first element of t. It raises Empty if t is empty.
	*)
	val tl: 'a sequence -> 'a sequence

	(*
		append (t, u) returns a sequence of the elements
		of t, followed by the elements of u.
	*)
	val append: 'a sequence * 'a sequence -> 'a sequence

	(*
		filter f t returns a sequence of only the elements
		of t in which f returns true.
	*)
	val filter: ('a -> bool) -> 'a sequence -> 'a sequence

	(*
		mapPartial f t returns a sequence of only the elements
		where the application of f returns SOME, with the SOME
		stripped out.
	*)
	val mapPartial: ('a -> 'b option) -> 'a sequence -> 'b sequence

	(*
		zip (t, u) returns a sequence of pairs of the elements of
		t and u. If t and u have unequal lengths, the returned
		sequence will have only as many elements as the shorter
		sequence, and the extra elements from the longer sequence
		will be ignored.
	*)
	val zip: 'a sequence * 'b sequence -> ('a * 'b) sequence

	(*
		unzip t returns a pair of sequences. The first sequence
		contains the first element of each pair in t, while the
		second sequence contains the second element of each pair
		in t.
	*)
	val unzip: ('a * 'b) sequence -> 'a sequence * 'b sequence

	(*
		concat t returns a sequence that is a concatenation of
		all the sequences in t.
	*)
	val concat: 'a sequence sequence -> 'a sequence

	(*
		concatWith t u returns a sequence that is a concatentation
		of all the sequences in u, with t inserted in between
		them.
	*)
	val concatWith: 'a sequence -> 'a sequence sequence -> 'a sequence

	(*
		partition f t returns a pair of sequences. The first
		sequence in the pair consists of elements which, when
		applied to f, yield true, and the second sequence
		consists of the the elements which, when applied to f,
		yield false.
	*)
	val partition: ('a -> bool) -> 'a sequence -> 'a sequence * 'a sequence

	(*
		rev t returns a sequence with the elements of t
		in reverse order.
	*)
	val rev: 'a sequence -> 'a sequence
end

signature PURE_SEQUENCE = sig
	type 'a pureSequence

	(*
		update (t, i, x) returns a new sequence, with
		the ith element in t replaced by x.
	*)
	val update: 'a pureSequence * int * 'a -> 'a pureSequence
end

signature IMPURE_SEQUENCE = sig
	type 'a impureSequence

	(*
		update (t, i, x) changes the element at index i
		in sequence t to x.
	*)
	val update: 'a impureSequence * int * 'a -> unit

	(*
		modify f t replaces each element of sequence t
		with the result of applying f to the element.
	*)
	val modify: ('a -> 'a) -> 'a impureSequence -> unit

	(*
		modifyi f t replaces each element of sequence t
		with the result of applying f to the index of
		the element and the element itself.
	*)
	val modifyi: (int * 'a -> 'a) -> 'a impureSequence -> unit

	val copy: {src: 'a impureSequence, dst: 'a impureSequence, di: int} -> unit
	val copyRev: {src: 'a impureSequence, dst: 'a impureSequence, di: int} -> unit
	val copyShadow: {src: 'a Shadow.t, dst: 'a impureSequence, di: int} -> unit
	val copyRevShadow: {src: 'a Shadow.t, dst: 'a impureSequence, di: int} -> unit
	val copyArray: {src: 'a array, dst: 'a impureSequence, di: int} -> unit
	val copyRevArray: {src: 'a array, dst: 'a impureSequence, di: int} -> unit
	val copyCharArray: {src: CharArray.array, dst: char impureSequence, di: int} -> unit
	val copyRevCharArray: {src: CharArray.array, dst: char impureSequence, di: int} -> unit
	val copyBoolArray: {src: BoolArray.array, dst: bool impureSequence, di: int} -> unit
	val copyRevBoolArray: {src: BoolArray.array, dst: bool impureSequence, di: int} -> unit
	val copyIntArray: {src: IntArray.array, dst: int impureSequence, di: int} -> unit
	val copyRevIntArray: {src: IntArray.array, dst: int impureSequence, di: int} -> unit
	val copyWordArray: {src: WordArray.array, dst: word impureSequence, di: int} -> unit
	val copyRevWordArray: {src: WordArray.array, dst: word impureSequence, di: int} -> unit
	val copyRealArray: {src: RealArray.array, dst: real impureSequence, di: int} -> unit
	val copyRevRealArray: {src: RealArray.array, dst: real impureSequence, di: int} -> unit
	val copyLargeIntArray: {src: LargeIntArray.array, dst: LargeInt.int impureSequence, di: int}
		-> unit
	val copyRevLargeIntArray: {
		src: LargeIntArray.array
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyLargeWordArray: {
		src: LargeWordArray.array
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyRevLargeWordArray: {
		src: LargeWordArray.array
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyInt8Array: {src: Int8Array.array, dst: Int8.int impureSequence, di: int} -> unit
	val copyRevInt8Array: {src: Int8Array.array, dst: Int8.int impureSequence, di: int} -> unit
	val copyInt16Array: {src: Int16Array.array, dst: Int16.int impureSequence, di: int} -> unit
	val copyRevInt16Array: {src: Int16Array.array, dst: Int16.int impureSequence, di: int}
		-> unit
	val copyInt32Array: {src: Int32Array.array, dst: Int32.int impureSequence, di: int} -> unit
	val copyRevInt32Array: {src: Int32Array.array, dst: Int32.int impureSequence, di: int}
		-> unit
	val copyInt64Array: {src: Int64Array.array, dst: Int64.int impureSequence, di: int} -> unit
	val copyRevInt64Array: {src: Int64Array.array, dst: Int64.int impureSequence, di: int}
		-> unit
	val copyWord8Array: {src: Word8Array.array, dst: Word8.word impureSequence, di: int} -> unit
	val copyRevWord8Array: {src: Word8Array.array, dst: Word8.word impureSequence, di: int}
		-> unit
	val copyWord16Array: {src: Word16Array.array, dst: Word16.word impureSequence, di: int}
		-> unit
	val copyRevWord16Array: {src: Word16Array.array, dst: Word16.word impureSequence, di: int}
		-> unit
	val copyWord32Array: {src: Word32Array.array, dst: Word32.word impureSequence, di: int}
		-> unit
	val copyRevWord32Array: {src: Word32Array.array, dst: Word32.word impureSequence, di: int}
		-> unit
	val copyWord64Array: {src: Word64Array.array, dst: Word64.word impureSequence, di: int}
		-> unit
	val copyRevWord64Array: {src: Word64Array.array, dst: Word64.word impureSequence, di: int}
		-> unit
	val copyReal32Array: {src: Real32Array.array, dst: Real32.real impureSequence, di: int}
		-> unit
	val copyRevReal32Array: {src: Real32Array.array, dst: Real32.real impureSequence, di: int}
		-> unit
	val copyReal64Array: {src: Real64Array.array, dst: Real64.real impureSequence, di: int}
		-> unit
	val copyRevReal64Array: {src: Real64Array.array, dst: Real64.real impureSequence, di: int}
		-> unit

	val copyArraySlice: {src: 'a ArraySlice.slice, dst: 'a impureSequence, di: int} -> unit
	val copyRevArraySlice: {src: 'a ArraySlice.slice, dst: 'a impureSequence, di: int} -> unit
	val copyCharArraySlice: {src: CharArraySlice.slice, dst: char impureSequence, di: int}
		-> unit
	val copyRevCharArraySlice: {src: CharArraySlice.slice, dst: char impureSequence, di: int}
		-> unit
	val copyBoolArraySlice: {src: BoolArraySlice.slice, dst: bool impureSequence, di: int}
		-> unit
	val copyRevBoolArraySlice: {src: BoolArraySlice.slice, dst: bool impureSequence, di: int}
		-> unit
	val copyIntArraySlice: {src: IntArraySlice.slice, dst: int impureSequence, di: int} -> unit
	val copyRevIntArraySlice: {src: IntArraySlice.slice, dst: int impureSequence, di: int}
		-> unit
	val copyWordArraySlice: {src: WordArraySlice.slice, dst: word impureSequence, di: int}
		-> unit
	val copyRevWordArraySlice: {src: WordArraySlice.slice, dst: word impureSequence, di: int}
		-> unit
	val copyRealArraySlice: {src: RealArraySlice.slice, dst: real impureSequence, di: int}
		-> unit
	val copyRevRealArraySlice: {src: RealArraySlice.slice, dst: real impureSequence, di: int}
		-> unit
	val copyLargeIntArraySlice: {
		src: LargeIntArraySlice.slice
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyRevLargeIntArraySlice: {
		src: LargeIntArraySlice.slice
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyLargeWordArraySlice: {
		src: LargeWordArraySlice.slice
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyRevLargeWordArraySlice: {
		src: LargeWordArraySlice.slice
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyInt8ArraySlice: {src: Int8ArraySlice.slice, dst: Int8.int impureSequence, di: int}
		-> unit
	val copyRevInt8ArraySlice: {src: Int8ArraySlice.slice, dst: Int8.int impureSequence, di: int}
		-> unit
	val copyInt16ArraySlice: {src: Int16ArraySlice.slice, dst: Int16.int impureSequence, di: int}
		-> unit
	val copyRevInt16ArraySlice: {
		src: Int16ArraySlice.slice
		, dst: Int16.int impureSequence
		, di: int
	} -> unit
	val copyInt32ArraySlice: {
		src: Int32ArraySlice.slice
		, dst: Int32.int impureSequence
		, di: int
	} -> unit
	val copyRevInt32ArraySlice: {
		src: Int32ArraySlice.slice
		, dst: Int32.int impureSequence
		, di: int
	} -> unit
	val copyInt64ArraySlice: {
		src: Int64ArraySlice.slice
		, dst: Int64.int impureSequence
		, di: int
	} -> unit
	val copyRevInt64ArraySlice: {
		src: Int64ArraySlice.slice
		, dst: Int64.int impureSequence
		, di: int
	} -> unit
	val copyWord8ArraySlice: {
		src: Word8ArraySlice.slice
		, dst: Word8.word impureSequence
		, di: int
	} -> unit
	val copyRevWord8ArraySlice: {
		src: Word8ArraySlice.slice
		, dst: Word8.word impureSequence
		, di: int
	} -> unit
	val copyWord16ArraySlice: {
		src: Word16ArraySlice.slice
		, dst: Word16.word impureSequence
		, di: int
	} -> unit
	val copyRevWord16ArraySlice: {
		src: Word16ArraySlice.slice
		, dst: Word16.word impureSequence
		, di: int
	} -> unit
	val copyWord32ArraySlice: {
		src: Word32ArraySlice.slice
		, dst: Word32.word impureSequence
		, di: int
	} -> unit
	val copyRevWord32ArraySlice: {
		src: Word32ArraySlice.slice
		, dst: Word32.word impureSequence
		, di: int
	} -> unit
	val copyWord64ArraySlice: {
		src: Word64ArraySlice.slice
		, dst: Word64.word impureSequence
		, di: int
	} -> unit
	val copyRevWord64ArraySlice: {
		src: Word64ArraySlice.slice
		, dst: Word64.word impureSequence
		, di: int
	} -> unit
	val copyReal32ArraySlice: {
		src: Real32ArraySlice.slice
		, dst: Real32.real impureSequence
		, di: int
	} -> unit
	val copyRevReal32ArraySlice: {
		src: Real32ArraySlice.slice
		, dst: Real32.real impureSequence
		, di: int
	} -> unit
	val copyReal64ArraySlice: {
		src: Real64ArraySlice.slice
		, dst: Real64.real impureSequence
		, di: int
	} -> unit
	val copyRevReal64ArraySlice: {
		src: Real64ArraySlice.slice
		, dst: Real64.real impureSequence
		, di: int
	} -> unit

	val copyVec: {src: 'a vector, dst: 'a impureSequence, di: int} -> unit
	val copyVector: {src: 'a vector, dst: 'a impureSequence, di: int} -> unit
	val copyRevVector: {src: 'a vector, dst: 'a impureSequence, di: int} -> unit
	val copyCharVector: {src: CharVector.vector, dst: char impureSequence, di: int} -> unit
	val copyRevCharVector: {src: CharVector.vector, dst: char impureSequence, di: int} -> unit
	val copyBoolVector: {src: BoolVector.vector, dst: bool impureSequence, di: int} -> unit
	val copyRevBoolVector: {src: BoolVector.vector, dst: bool impureSequence, di: int} -> unit
	val copyIntVector: {src: IntVector.vector, dst: int impureSequence, di: int} -> unit
	val copyRevIntVector: {src: IntVector.vector, dst: int impureSequence, di: int} -> unit
	val copyWordVector: {src: WordVector.vector, dst: word impureSequence, di: int} -> unit
	val copyRevWordVector: {src: WordVector.vector, dst: word impureSequence, di: int} -> unit
	val copyRealVector: {src: RealVector.vector, dst: real impureSequence, di: int} -> unit
	val copyRevRealVector: {src: RealVector.vector, dst: real impureSequence, di: int} -> unit
	val copyLargeIntVector: {
		src: LargeIntVector.vector
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyRevLargeIntVector: {
		src: LargeIntVector.vector
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyLargeWordVector: {
		src: LargeWordVector.vector
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyRevLargeWordVector: {
		src: LargeWordVector.vector
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyInt8Vector: {src: Int8Vector.vector, dst: Int8.int impureSequence, di: int} -> unit
	val copyRevInt8Vector: {src: Int8Vector.vector, dst: Int8.int impureSequence, di: int}
		-> unit
	val copyInt16Vector: {src: Int16Vector.vector, dst: Int16.int impureSequence, di: int}
		-> unit
	val copyRevInt16Vector: {src: Int16Vector.vector, dst: Int16.int impureSequence, di: int}
		-> unit
	val copyInt32Vector: {src: Int32Vector.vector, dst: Int32.int impureSequence, di: int}
		-> unit
	val copyRevInt32Vector: {src: Int32Vector.vector, dst: Int32.int impureSequence, di: int}
		-> unit
	val copyInt64Vector: {src: Int64Vector.vector, dst: Int64.int impureSequence, di: int}
		-> unit
	val copyRevInt64Vector: {src: Int64Vector.vector, dst: Int64.int impureSequence, di: int}
		-> unit
	val copyWord8Vector: {src: Word8Vector.vector, dst: Word8.word impureSequence, di: int}
		-> unit
	val copyRevWord8Vector: {src: Word8Vector.vector, dst: Word8.word impureSequence, di: int}
		-> unit
	val copyWord16Vector: {src: Word16Vector.vector, dst: Word16.word impureSequence, di: int}
		-> unit
	val copyRevWord16Vector: {src: Word16Vector.vector, dst: Word16.word impureSequence, di: int}
		-> unit
	val copyWord32Vector: {src: Word32Vector.vector, dst: Word32.word impureSequence, di: int}
		-> unit
	val copyRevWord32Vector: {src: Word32Vector.vector, dst: Word32.word impureSequence, di: int}
		-> unit
	val copyWord64Vector: {src: Word64Vector.vector, dst: Word64.word impureSequence, di: int}
		-> unit
	val copyRevWord64Vector: {src: Word64Vector.vector, dst: Word64.word impureSequence, di: int}
		-> unit
	val copyReal32Vector: {src: Real32Vector.vector, dst: Real32.real impureSequence, di: int}
		-> unit
	val copyRevReal32Vector: {src: Real32Vector.vector, dst: Real32.real impureSequence, di: int}
		-> unit
	val copyReal64Vector: {src: Real64Vector.vector, dst: Real64.real impureSequence, di: int}
		-> unit
	val copyRevReal64Vector: {src: Real64Vector.vector, dst: Real64.real impureSequence, di: int}
		-> unit

	val copyVectorSlice: {src: 'a VectorSlice.slice, dst: 'a impureSequence, di: int} -> unit
	val copyRevVectorSlice: {src: 'a VectorSlice.slice, dst: 'a impureSequence, di: int} -> unit
	val copyCharVectorSlice: {src: CharVectorSlice.slice, dst: char impureSequence, di: int}
		-> unit
	val copyRevCharVectorSlice: {src: CharVectorSlice.slice, dst: char impureSequence, di: int}
		-> unit
	val copyBoolVectorSlice: {src: BoolVectorSlice.slice, dst: bool impureSequence, di: int}
		-> unit
	val copyRevBoolVectorSlice: {src: BoolVectorSlice.slice, dst: bool impureSequence, di: int}
		-> unit
	val copyIntVectorSlice: {src: IntVectorSlice.slice, dst: int impureSequence, di: int} -> unit
	val copyRevIntVectorSlice: {src: IntVectorSlice.slice, dst: int impureSequence, di: int}
		-> unit
	val copyWordVectorSlice: {src: WordVectorSlice.slice, dst: word impureSequence, di: int}
		-> unit
	val copyRevWordVectorSlice: {src: WordVectorSlice.slice, dst: word impureSequence, di: int}
		-> unit
	val copyRealVectorSlice: {src: RealVectorSlice.slice, dst: real impureSequence, di: int}
		-> unit
	val copyRevRealVectorSlice: {src: RealVectorSlice.slice, dst: real impureSequence, di: int}
		-> unit
	val copyLargeIntVectorSlice: {
		src: LargeIntVectorSlice.slice
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyRevLargeIntVectorSlice: {
		src: LargeIntVectorSlice.slice
		, dst: LargeInt.int impureSequence
		, di: int
	} -> unit
	val copyLargeWordVectorSlice: {
		src: LargeWordVectorSlice.slice
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyRevLargeWordVectorSlice: {
		src: LargeWordVectorSlice.slice
		, dst: LargeWord.word impureSequence
		, di: int
	} -> unit
	val copyInt8VectorSlice: {src: Int8VectorSlice.slice, dst: Int8.int impureSequence, di: int}
		-> unit
	val copyRevInt8VectorSlice: {
		src: Int8VectorSlice.slice
		, dst: Int8.int impureSequence
		, di: int
	} -> unit
	val copyInt16VectorSlice: {
		src: Int16VectorSlice.slice
		, dst: Int16.int impureSequence
		, di: int
	} -> unit
	val copyRevInt16VectorSlice: {
		src: Int16VectorSlice.slice
		, dst: Int16.int impureSequence
		, di: int
	} -> unit
	val copyInt32VectorSlice: {
		src: Int32VectorSlice.slice
		, dst: Int32.int impureSequence
		, di: int
	} -> unit
	val copyRevInt32VectorSlice: {
		src: Int32VectorSlice.slice
		, dst: Int32.int impureSequence
		, di: int
	} -> unit
	val copyInt64VectorSlice: {
		src: Int64VectorSlice.slice
		, dst: Int64.int impureSequence
		, di: int
	} -> unit
	val copyRevInt64VectorSlice: {
		src: Int64VectorSlice.slice
		, dst: Int64.int impureSequence
		, di: int
	} -> unit
	val copyWord8VectorSlice: {
		src: Word8VectorSlice.slice
		, dst: Word8.word impureSequence
		, di: int
	} -> unit
	val copyRevWord8VectorSlice: {
		src: Word8VectorSlice.slice
		, dst: Word8.word impureSequence
		, di: int
	} -> unit
	val copyWord16VectorSlice: {
		src: Word16VectorSlice.slice
		, dst: Word16.word impureSequence
		, di: int
	} -> unit
	val copyRevWord16VectorSlice: {
		src: Word16VectorSlice.slice
		, dst: Word16.word impureSequence
		, di: int
	} -> unit
	val copyWord32VectorSlice: {
		src: Word32VectorSlice.slice
		, dst: Word32.word impureSequence
		, di: int
	} -> unit
	val copyRevWord32VectorSlice: {
		src: Word32VectorSlice.slice
		, dst: Word32.word impureSequence
		, di: int
	} -> unit
	val copyWord64VectorSlice: {
		src: Word64VectorSlice.slice
		, dst: Word64.word impureSequence
		, di: int
	} -> unit
	val copyRevWord64VectorSlice: {
		src: Word64VectorSlice.slice
		, dst: Word64.word impureSequence
		, di: int
	} -> unit
	val copyReal32VectorSlice: {
		src: Real32VectorSlice.slice
		, dst: Real32.real impureSequence
		, di: int
	} -> unit
	val copyRevReal32VectorSlice: {
		src: Real32VectorSlice.slice
		, dst: Real32.real impureSequence
		, di: int
	} -> unit
	val copyReal64VectorSlice: {
		src: Real64VectorSlice.slice
		, dst: Real64.real impureSequence
		, di: int
	} -> unit
	val copyRevReal64VectorSlice: {
		src: Real64VectorSlice.slice
		, dst: Real64.real impureSequence
		, di: int
	} -> unit
	val copyList: {src: 'a list, dst: 'a impureSequence, di: int} -> unit
	val copyTextIO: {src: TextIO.instream, dst: char impureSequence, di: int} -> unit
	val copyTextIOLines: {src: TextIO.instream, dst: string impureSequence, di: int} -> unit
	val copyBinIO: {src: BinIO.instream, dst: Word8.word impureSequence, di: int} -> unit
end
