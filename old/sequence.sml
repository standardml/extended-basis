signature SEQUENCE = sig
	type 'a t
	val create: {
		next: unit -> 'a option
		, length: unit -> int
		, clone: unit -> 'a t
	} -> 'a t
	val augment: {
		next: unit -> 'a option
		, length: (unit -> int) option
		, clone: (unit -> 'a t) option
	} -> 'a t
	val next: 'a t -> 'a option
	val length: 'a t -> int
	val clone: 'a t -> 'a t
	val empty: unit -> 'a t
	val single: 'a -> 'a t
	val unfold: ('a -> ('b * 'a) option) -> 'a -> 'b t
	val for: {
		start: 'a
		, test: 'a -> bool
		, step: 'a -> 'a
	} -> 'a t
	val numbers: {from: int , to: int , step: int} -> int t
	val fromArray: 'a array -> 'a t
	val fromCharArray: CharArray.array -> char t
	val fromBoolArray: BoolArray.array -> bool t
	val fromIntArray: IntArray.array -> int t
	val fromWordArray: WordArray.array -> word t
	val fromRealArray: RealArray.array -> real t
	val fromLargeIntArray: LargeIntArray.array -> LargeInt.int t
	val fromLargeWordArray: LargeWordArray.array -> LargeWord.word t
	val fromInt8Array: Int8Array.array -> Int8.int t
	val fromInt16Array: Int16Array.array -> Int16.int t
	val fromInt32Array: Int32Array.array -> Int32.int t
	val fromInt64Array: Int64Array.array -> Int64.int t
	val fromWord8Array: Word8Array.array -> Word8.word t
	val fromWord16Array: Word16Array.array -> Word16.word t
	val fromWord32Array: Word32Array.array -> Word32.word t
	val fromWord64Array: Word64Array.array -> Word64.word t
	val fromReal32Array: Real32Array.array -> Real32.real t
	val fromReal64Array: Real64Array.array -> Real64.real t
	val fromArraySlice: 'a ArraySlice.slice -> 'a t
	val fromCharArraySlice: CharArraySlice.slice -> char t
	val fromBoolArraySlice: BoolArraySlice.slice -> bool t
	val fromIntArraySlice: IntArraySlice.slice -> int t
	val fromWordArraySlice: WordArraySlice.slice -> word t
	val fromRealArraySlice: RealArraySlice.slice -> real t
	val fromLargeIntArraySlice: LargeIntArraySlice.slice -> LargeInt.int t
	val fromLargeWordArraySlice: LargeWordArraySlice.slice -> LargeWord.word t
	val fromInt8ArraySlice: Int8ArraySlice.slice -> Int8.int t
	val fromInt16ArraySlice: Int16ArraySlice.slice -> Int16.int t
	val fromInt32ArraySlice: Int32ArraySlice.slice -> Int32.int t
	val fromInt64ArraySlice: Int64ArraySlice.slice -> Int64.int t
	val fromWord8ArraySlice: Word8ArraySlice.slice -> Word8.word t
	val fromWord16ArraySlice: Word16ArraySlice.slice -> Word16.word t
	val fromWord32ArraySlice: Word32ArraySlice.slice -> Word32.word t
	val fromWord64ArraySlice: Word64ArraySlice.slice -> Word64.word t
	val fromVector: 'a vector -> 'a t
	val fromCharVector: CharVector.vector -> char t
	val fromBoolVector: BoolVector.vector -> bool t
	val fromIntVector: IntVector.vector -> int t
	val fromWordVector: WordVector.vector -> word t
	val fromRealVector: RealVector.vector -> real t
	val fromLargeIntVector: LargeIntVector.vector -> LargeInt.int t
	val fromLargeWordVector: LargeWordVector.vector -> LargeWord.word t
	val fromInt8Vector: Int8Vector.vector -> Int8.int t
	val fromInt16Vector: Int16Vector.vector -> Int16.int t
	val fromInt32Vector: Int32Vector.vector -> Int32.int t
	val fromInt64Vector: Int64Vector.vector -> Int64.int t
	val fromWord8Vector: Word8Vector.vector -> Word8.word t
	val fromWord16Vector: Word16Vector.vector -> Word16.word t
	val fromWord32Vector: Word32Vector.vector -> Word32.word t
	val fromWord64Vector: Word64Vector.vector -> Word64.word t
	val fromReal32Vector: Real32Vector.vector -> Real32.real t
	val fromReal64Vector: Real64Vector.vector -> Real64.real t
	val fromVectorSlice: 'a VectorSlice.slice -> 'a t
	val fromCharVectorSlice: CharVectorSlice.slice -> char t
	val fromBoolVectorSlice: BoolVectorSlice.slice -> bool t
	val fromIntVectorSlice: IntVectorSlice.slice -> int t
	val fromWordVectorSlice: WordVectorSlice.slice -> word t
	val fromRealVectorSlice: RealVectorSlice.slice -> real t
	val fromLargeIntVectorSlice: LargeIntVectorSlice.slice -> LargeInt.int t
	val fromLargeWordVectorSlice: LargeWordVectorSlice.slice -> LargeWord.word t
	val fromInt8VectorSlice: Int8VectorSlice.slice -> Int8.int t
	val fromInt16VectorSlice: Int16VectorSlice.slice -> Int16.int t
	val fromInt32VectorSlice: Int32VectorSlice.slice -> Int32.int t
	val fromInt64VectorSlice: Int64VectorSlice.slice -> Int64.int t
	val fromWord8VectorSlice: Word8VectorSlice.slice -> Word8.word t
	val fromWord16VectorSlice: Word16VectorSlice.slice -> Word16.word t
	val fromWord32VectorSlice: Word32VectorSlice.slice -> Word32.word t
	val fromWord64VectorSlice: Word64VectorSlice.slice -> Word64.word t
	val fromString: string -> char t
	val fromSubstring: Substring.substring -> char t
	val fromList: 'a list -> 'a t
	val fromTextIO: TextIO.instream -> char t
	val fromTextIOLines: TextIO.instream -> string t
	val fromBinIO: BinIO.instream -> Word8.word t
	val toList: 'a t -> 'a list
	val toArray: 'a t -> 'a array
	val toCharArray: char t -> CharArray.array
	val toBoolArray: bool t -> BoolArray.array
	val toIntArray: int t -> IntArray.array
	val toWordArray: word t -> WordArray.array
	val toRealArray: real t -> RealArray.array
	val toLargeIntArray: LargeInt.int t -> LargeIntArray.array
	val toLargeWordArray: LargeWord.word t -> LargeWordArray.array
	val toInt8Array: Int8.int t -> Int8Array.array
	val toInt16Array: Int16.int t -> Int16Array.array
	val toInt32Array: Int32.int t -> Int32Array.array
	val toInt64Array: Int64.int t -> Int64Array.array
	val toWord8Array: Word8.word t -> Word8Array.array
	val toWord16Array: Word16.word t -> Word16Array.array
	val toWord32Array: Word32.word t -> Word32Array.array
	val toWord64Array: Word64.word t -> Word64Array.array
	val toReal32Array: Real32.real t -> Real32Array.array
	val toReal64Array: Real64.real t -> Real64Array.array
	val toVector: 'a t -> 'a vector
	val toCharVector: char t -> CharVector.vector
	val toBoolVector: bool t -> BoolVector.vector
	val toIntVector: int t -> IntVector.vector
	val toWordVector: word t -> WordVector.vector
	val toRealVector: real t -> RealVector.vector
	val toLargeIntVector: LargeInt.int t -> LargeIntVector.vector
	val toLargeWordVector: LargeWord.word t -> LargeWordVector.vector
	val toInt8Vector: Int8.int t -> Int8Vector.vector
	val toInt16Vector: Int16.int t -> Int16Vector.vector
	val toInt32Vector: Int32.int t -> Int32Vector.vector
	val toInt64Vector: Int64.int t -> Int64Vector.vector
	val toWord8Vector: Word8.word t -> Word8Vector.vector
	val toWord16Vector: Word16.word t -> Word16Vector.vector
	val toWord32Vector: Word32.word t -> Word32Vector.vector
	val toWord64Vector: Word64.word t -> Word64Vector.vector
	val toReal32Vector: Real32.real t -> Real32Vector.vector
	val toReal64Vector: Real64.real t -> Real64Vector.vector
	val toString: char t -> string
	val append: 'a t * 'a t -> 'a t
	val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
	val map: ('a -> 'b) -> 'a t -> 'b t
	val app: ('a -> unit) -> 'a t -> unit
	val filter: ('a -> bool) -> 'a t -> 'a t
	val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
	val zip: 'a t * 'b t -> ('a * 'b) t
	val unzip: ('a * 'b) t -> 'a t * 'b t
	val concat: 'a t t -> 'a t
	val pairs: 'a t -> ('a * 'a) t
	val find: ('a -> bool) -> 'a t -> 'a option
	val groups: ('a t * int) -> 'a t t
	val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
	val rev: 'a t -> 'a t
	val exists: ('a -> bool) -> 'a t -> bool
	val all: ('a -> bool) -> 'a t -> bool
	val tabulate: int * (int -> 'a) -> 'a t
	val drop: 'a t * int -> unit
	val take: 'a t * int -> 'a t
	val tokens: ('a -> bool) -> 'a t -> 'a t t
	val fields: ('a -> bool) -> 'a t -> 'a t t
	val translate: ('a -> 'a t) -> 'a t -> 'a t
end

structure Sequence :> SEQUENCE = struct
	datatype 'a t = T of {
		next: unit -> 'a option
		, length: unit -> int
		, clone: unit -> 'a t
	}
	fun create r = T r
	fun augment
	fun next (T {next, ...}) = next ()
	fun length (T {length, ...}) = length ()
	fun clone (T {clone, ...}) = clone ()
	fun empty () = create {length = fn () => 0, next = fn () => NONE, clone = empty}
	fun single x =
		let
			fun length' y () = case !y of
				NONE => 0
				| SOME _ => 1
			fun next' y () = case !y of
				NONE => NONE
				| z as (SOME _) => (
					y := NONE
					; z
				)
			fun clone' y () =
				let
					val z = ref (!y)
				in
					create {
						length = length' z
						, next = next' z
						, clone = clone' z
					}
				end
			val y = ref (SOME x)
		in
			create {
				length = length' y
				, next = next' y
				, clone = clone' y
			}
		end
	fun for {start, test, step} = unfold
		(fn x =>
			if test x then SOME (x, step x)
			else NONE
		) start
	fun numbers {from, to, step} =
		let
			fun length' index () = case !index of
				NONE => 0
				| SOME x => (to - x) div step + 1
			fun next' index () = case !index of
				NONE => NONE
				| x as (SOME y) =>
					let
						val z = y + step
					in
						if
							(step > 0 andalso z > to)
							orelse (step < 0 andalso z < to)
						then index := NONE
						else index := SOME z
						; y
					end
			fun clone' index () =
				let
					val index = ref (!index)
				in
					create {
						length = length' index
						, next = next' index
						, clone = clone' index
					}
				end
			val index = ref start
		in
			create {
				length = length' index
				, next = next' index
				, clne = clone' index
			}
		end
	structure Fifo :> sig
		type 'a t
		val empty: 'a t
		val length: 'a t -> int
		val push: 'a t * 'a -> 'a t
		val pop: 'a t -> ('a t * 'a) option
	end = struct
		type 'a t = int * 'a list * 'a list
		val empty = (0, nil, nil)
		fun push ((count, left, right), item) = (count + 1, left, item :: right)
		fun pop (count, left, right) = case (left, right) of
			(nil, nil) => NONE
			| (nil, _) => (
				case rev right of
					nil => raise Fail "impossible"
					| x :: y => SOME ((count - 1, y, nil), x)
			) | (x :: y, _) => SOME ((count - 1, y, right), x)
		fun length (count, _, _) = count
	end
	fun fromFifo (fifo: 'a Fifo.t): 'a t =
		let
			fun length fifo () = Fifo.length (!fifo)
			fun next fifo () = case Fifo.pop (!fifo) of
				NONE => NONE
				| SOME (new, x) => (
					fifo := new
					; SOME x
				)
			fun clone fifo () =
				let
					val fifo = ref (!fifo)
				in
					create {
						length = length fifo
						, next = next fifo
						, clone = clone fifo
					}
				end
			val fifo = ref fifo
		in
			create {
				length = length fifo
				, next = next fifo
				, clone = clone fifo
			}
		end
	datatype 'a forceable = Unforced | Forced of 'a Fifo.t ref
	fun forceOnLength {next, clone} =
		let
			fun force forceable =
				let
					fun loop fifo = case next () of
						NONE => forceable := Forced (ref fifo)
						| SOME x => loop (Fifo.push x)
				in
					loop Fifo.empty
				end
			fun length' forceable () = case !forceable of
				Unforced => (
					force forceable
					; length' forceable ()
				) | Forced (ref fifo) => Fifo.length fifo
			fun next' forceable () = case !forceable of
				Unforced => next ()
				| Forced fifo => (case Fifo.pop (!fifo) of
					NONE => NONE
					| SOME (new, x) => x before fifo := new
				)
			fun clone' forceable () = case !forceable of
				Unforced => clone ()
				| Forced fifo => fromFifo fifo
			val forceable = ref Unforced
		in
			create {
				length = length' forceable
				, next = next' forceable
				, clone = clone' forceable
			}
		end
	fun forceOnClone {next, length} =
		let
			fun force forceable =
				let
					fun loop fifo = case next () of
						NONE => forceable := Forced (ref fifo)
						| SOME x => loop (Fifo.push x)
				in
					loop Fifo.empty
				end
			fun length' forceable () = case !forceable of
				Unforced => length ()
				| Forced (ref fifo) => Fifo.length fifo
			fun clone' forceable () = case !forceable of
				
		in
		end
	fun forceOnEither f =
		let
			fun force forceable =
				let
					fun loop fifo = case f () of
						NONE => forceable := Forced (ref fifo)
						| SOME x => loop (Fifo.push (fifo, x))
				in
					loop Fifo.empty
				end
			fun next' forceable () = case !forceable of
				Unforced => f ()
				| Forced fifo => (case Fifo.pop !fifo of
					NONE => NONE
					| SOME (new, x) => (
						fifo := new
						; x
					)
				)
			fun length' forceable () = case !forceable of
				Unforced => (
					force forceable
					; length' forceable ()
				) | Forced (ref fifo) => Fifo.length fifo
			fun clone' forceable () = case !forceable of
				Unforced => (
					force forceable
					; clone' forceable ()
				) | Forced (ref fifo) =>
					let
						val y = ref (
							Forced {
								length = ref (!length)
								, contents = ref (!contents)
							}
						)
					in
						create {
							next = next' y
							, length = length' y
							, clone = clone' y
						}
					end
			val forceable = ref Unforced
		in create {
			next = next' forceable
			, length = length' forceable
			, clone = clone' forceable
		} end
	fun fromGeneric (genericLength, genericSub) x =
		let
			fun length i () = genericLength x - !i
			fun next i () =
				if !i >= genericLength x then NONE
				else
					SOME (genericSub (x, !i))
					before i := !i + 1
			fun clone i () =
				let
					val j = ref (!i)
				in T {
					length = length j
					, next = next j
					, clone = clone j
				} end
			val i = ref 0
		in
			create {
				length = length i
				, next = next i
				, clone = clone i
			}
		end
	fun fromArray array = fromGeneric (Array.length, Array.sub) array
	val fromCharArray = fromGeneric (CharArray.length, CharArray.sub)
	val fromBoolArray = fromGeneric (BoolArray.length, BoolArray.sub)
	val fromIntArray = fromGeneric (IntArray.length, IntArray.sub)
	val fromWordArray = fromGeneric (WordArray.length, WordArray.sub)
	val fromRealArray = fromGeneric (RealArray.length, RealArray.sub)
	val fromLargeIntArray = fromGeneric (LargeIntArray.length, LargeIntArray.sub)
	val fromLargeWordArray = fromGeneric (LargeWordArray.length, LargeWordArray.sub)
	val fromInt8Array = fromGeneric (Int8Array.length, Int8Array.sub)
	val fromInt16Array = fromGeneric (Int16Array.length, Int16Array.sub)
	val fromInt32Array = fromGeneric (Int32Array.length, Int32Array.sub)
	val fromInt64Array = fromGeneric (Int64Array.length, Int64Array.sub)
	val fromWord8Array = fromGeneric (Word8Array.length, Word8Array.sub)
	val fromWord16Array = fromGeneric (Word16Array.length, Word16Array.sub)
	val fromWord32Array = fromGeneric (Word32Array.length, Word32Array.sub)
	val fromWord64Array = fromGeneric (Word64Array.length, Word64Array.sub)
	val fromReal32Array = fromGeneric (Real32Array.length, Real32Array.sub)
	val fromReal64Array = fromGeneric (Real64Array.length, Real64Array.sub)
	fun fromArraySlice slice = fromGeneric (ArraySlice.length, ArraySlice.sub) slice
	val fromCharArraySlice = fromGeneric (CharArraySlice.length, CharArraySlice.sub)
	val fromBoolArraySlice = fromGeneric (BoolArraySlice.length, BoolArraySlice.sub)
	val fromIntArraySlice = fromGeneric (IntArraySlice.length, IntArraySlice.sub)
	val fromWordArraySlice = fromGeneric (WordArraySlice.length, WordArraySlice.sub)
	val fromRealArraySlice = fromGeneric (RealArraySlice.length, RealArraySlice.sub)
	val fromLargeIntArraySlice = fromGeneric (LargeIntArraySlice.length, LargeIntArraySlice.sub)
	val fromLargeWordArraySlice =
		fromGeneric (LargeWordArraySlice.length, LargeWordArraySlice.sub)
	val fromInt8ArraySlice = fromGeneric (Int8ArraySlice.length, Int8ArraySlice.sub)
	val fromInt16ArraySlice = fromGeneric (Int16ArraySlice.length, Int16ArraySlice.sub)
	val fromInt32ArraySlice = fromGeneric (Int32ArraySlice.length, Int32ArraySlice.sub)
	val fromInt64ArraySlice = fromGeneric (Int64ArraySlice.length, Int64ArraySlice.sub)
	val fromWord8ArraySlice = fromGeneric (Word8ArraySlice.length, Word8ArraySlice.sub)
	val fromWord16ArraySlice = fromGeneric (Word16ArraySlice.length, Word16ArraySlice.sub)
	val fromWord32ArraySlice = fromGeneric (Word32ArraySlice.length, Word32ArraySlice.sub)
	val fromWord64ArraySlice = fromGeneric (Word64ArraySlice.length, Word64ArraySlice.sub)
	fun fromVector vector = fromGeneric (Vector.length, Vector.sub) vector
	val fromCharVector = fromGeneric (CharVector.length, CharVector.sub)
	val fromBoolVector = fromGeneric (BoolVector.length, BoolVector.sub)
	val fromIntVector = fromGeneric (IntVector.length, IntVector.sub)
	val fromWordVector = fromGeneric (WordVector.length, WordVector.sub)
	val fromRealVector = fromGeneric (RealVector.length, RealVector.sub)
	val fromLargeIntVector = fromGeneric (LargeIntVector.length, LargeIntVector.sub)
	val fromLargeWordVector = fromGeneric (LargeWordVector.length, LargeWordVector.sub)
	val fromInt8Vector = fromGeneric (Int8Vector.length, Int8Vector.sub)
	val fromInt16Vector = fromGeneric (Int16Vector.length, Int16Vector.sub)
	val fromInt32Vector = fromGeneric (Int32Vector.length, Int32Vector.sub)
	val fromInt64Vector = fromGeneric (Int64Vector.length, Int64Vector.sub)
	val fromWord8Vector = fromGeneric (Word8Vector.length, Word8Vector.sub)
	val fromWord16Vector = fromGeneric (Word16Vector.length, Word16Vector.sub)
	val fromWord32Vector = fromGeneric (Word32Vector.length, Word32Vector.sub)
	val fromWord64Vector = fromGeneric (Word64Vector.length, Word64Vector.sub)
	val fromReal32Vector = fromGeneric (Real32Vector.length, Real32Vector.sub)
	val fromReal64Vector = fromGeneric (Real64Vector.length, Real64Vector.sub)
	fun fromVectorSlice slice = fromGeneric (VectorSlice.length, VectorSlice.sub) slice
	val fromCharVectorSlice = fromGeneric (CharVectorSlice.length, CharVectorSlice.sub)
	val fromBoolVectorSlice = fromGeneric (BoolVectorSlice.length, BoolVectorSlice.sub)
	val fromIntVectorSlice = fromGeneric (IntVectorSlice.length, IntVectorSlice.sub)
	val fromWordVectorSlice = fromGeneric (WordVectorSlice.length, WordVectorSlice.sub)
	val fromRealVectorSlice = fromGeneric (RealVectorSlice.length, RealVectorSlice.sub)
	val fromLargeIntVectorSlice =
		fromGeneric (LargeIntVectorSlice.length, LargeIntVectorSlice.sub)
	val fromLargeWordVectorSlice =
		fromGeneric (LargeWordVectorSlice.length, LargeWordVectorSlice.sub)
	val fromInt8VectorSlice = fromGeneric (Int8VectorSlice.length, Int8VectorSlice.sub)
	val fromInt16VectorSlice = fromGeneric (Int16VectorSlice.length, Int16VectorSlice.sub)
	val fromInt32VectorSlice = fromGeneric (Int32VectorSlice.length, Int32VectorSlice.sub)
	val fromInt64VectorSlice = fromGeneric (Int64VectorSlice.length, Int64VectorSlice.sub)
	val fromWord8VectorSlice = fromGeneric (Word8VectorSlice.length, Word8VectorSlice.sub)
	val fromWord16VectorSlice = fromGeneric (Word16VectorSlice.length, Word16VectorSlice.sub)
	val fromWord32VectorSlice = fromGeneric (Word32VectorSlice.length, Word32VectorSlice.sub)
	val fromWord64VectorSlice = fromGeneric (Word64VectorSlice.length, Word64VectorSlice.sub)
	val fromString = fromGeneric (String.size, String.sub)
	val fromSubstring = fromGeneric (Substring.size, Substring.sub)
	fun fromList list =
		let
			fun length' {length, list} () = case !length of
				NONE =>
					let
						val n = List.length (!list)
					in
						length := SOME n
						; n
					end
				| SOME n => n
			fun next' x () = case x of
				{list = ref nil, ...} => NONE
				| {list as ref (x :: y), length as ref (SOME n)} => (
					length := SOME (n - 1)
					; list := y
					; SOME x
				) | {list as ref (x as (y :: z)), length as ref NONE} => (
					length := SOME (List.length x)
					; list := z
					; SOME y
				)
			fun clone' {length, list} () =
				let
					val x = {length = ref (!length), list = ref (!list)}
				in
					create {
						length = length' x
						, next = next' x
						, clone = clone' x
					}
				end
			val x = {length = ref NONE, list = ref list}
		in
			create {
				length = length' x
				, next = next' x
				, clone = clone' x
			}
		end
	fun fromTextIO instream = from (fn () => TextIO.input1 instream)
	fun fromTextIOLines instream = from (fn () => TextIO.inputLine instream)
	fun fromBinIO instream = from (fn () => BinIO.input1 instream)
	fun fold f seed t =
		let
			fun loop x =
				case next t of
					NONE => x
					| SOME y => loop (f (y, x))
		in
			loop seed
		end
	fun append (a, b) =
		let
			val length = fn () => length a + length b
			val next = fn () => (
				case next a of
					NONE => next b
					| x => x
			)
			val clone = fn () => append (clone a, clone b)
		in
			create {
				length = length
				, next = next
				, clone = clone
			}
		end
	fun map f t =
		create {
			length = fn () => length t
			, next = fn () => Option.map f (next t)
			, clone = fn () => map f (clone t)
		}
	fun app f t =
		let
			fun loop () =
				case next t of
					NONE => ()
					| SOME x => (
						f x
						; loop ()
					)
		in
			loop ()
		end
	fun filter f t =
		let
			fun force forceable =
				let
					fun loop (n, l) = case next t of
						NONE => {length = ref n, contents = ref (rev l)}
						| SOME x =>
							if f x then loop (n + 1, x :: l)
							else loop (n, l)
				in
					forceable := Forced (loop (0, nil))
				end
			fun length' forceable () = case !forceable of
				Unforced => (
					force forceable
					; length' forceable ()
				) | Forced {length, ...} => !length
			fun next' forceable () = case !forceable of
				Unforced => (case next t of
					NONE => NONE
					| y as (SOME x) =>
						if f x then y
						else next' forceable ()
				) | Forced {contents as ref (x :: y), length} => (
					contents := y
					; length := !length - 1
					; SOME x
				) | Forced {contents = ref nil, ...} => NONE
			fun clone' forceable () = case !forceable of
				Unforced => filter f (clone t)
				| Forced {contents, length} =>
					let
						val forceable = ref (
							Forced {
								length = ref (!length)
								, contents = ref (!contents)
							}
						)
					in
						create {
							length = length' forceable
							, next = next' forceable
							, clone = clone' forceable
						}
					end
			val forceable = ref Unforced
		in
			create {
				length = length' forceable
				, next = next' forceable
				, clone = clone' forceable
			}
		end
	fun mapPartial f t =
		let
			fun force forceable =
				let
					fun loop (n, l) = case next t of
						NONE => {length = ref n, contents = ref (rev l)}
						| SOME x => (case f x of
							NONE => loop (n, l)
							| SOME y => loop (n + 1, y :: l)
						)
				in
					forceable := Forced (loop (0, nil))
				end
			fun length' forceable () = case !forceable of
				Unforced => (
					force forceable
					; length' forceable ()
				) | Forced {length, ...} => !length
			fun next' forceable () = case !forceable of
				Unforced => (case next t of
					NONE => NONE
					| SOME x => (case f x of
						NONE => next' forceable ()
						| y => y
					)
				) | Forced {contents = ref nil, ...} => NONE
				| Forced {contents as ref (x :: y), length} => (
					length := !length - 1
					; contents := y
					; SOME x
				)
			fun clone' forceable () = case !forceable of
				Unforced => mapPartial f (clone t)
				| Forced {contents, length} =>
					let
						val forceable = ref (
							Forced {
								length = ref (!length)
								, contents = ref (!contents)
							}
						)
					in
						create {
							length = length' forceable
							, next = next' forceable
							, clone = clone' forceable
						}
					end
			val forceable = ref Unforced
		in
			create {
				length = length' forceable
				, next = next' forceable
				, clone = clone' forceable
			}
		end
	fun zip (a, b) =
		create {
			length = fn () => Int.min (length a, length b)
			, next = fn () => (case (next a, next b) of
				(SOME x, SOME y) => SOME (x, y)
				| _ => NONE
			), clone = fn () => zip (clone a, clone b)
		}
	fun unzip t =
		let
			fun refill {left, right, source} = case next source of
				NONE => false
				| SOME (a, b) => (
					left := Fifo.push (!left, a)
					; right := Fifo.push (!right, b)
					; true
				)
			fun lengthLeft {left, right, source} () = length t + Fifo.length (!left)
			fun nextLeft (x as {left, right, source}) () = case Fifo.pop (!left) of
				NONE => if refill x then nextLeft x () else NONE
				| SOME (new, x) => (
					left := new
					; SOME x
				)
			fun cloneLeft {left, right, source} () =
				append (
					fromFifo (!left)
					, map (fn (x, _) => x) (clone t)
				)
			fun lengthRight {left, right, source} () = length t + Fifo.length (!right)
			fun nextRight (x as {left, right, source}) () = case Fifo.pop (!right) of
				NONE => if refill x then nextRight x () else NONE
				| SOME (new, x) => (
					right := new
					; SOME x
				)
			fun cloneRight {left, right, source} () =
				append (
					fromFifo (!right)
					, map (fn (_, x) => x) (clone t)
				)
			val x = {
				left = ref Fifo.empty
				, right = ref Fifo.empty
				, source = t
			}
		in
			(
				create {
					length = lengthLeft x
					, next = nextLeft x
					, clone = cloneLeft x
				}, create {
					length = lengthRight x
					, next = nextRight x
					, clone = cloneRight x
				}
			)
		end
	fun partition f t =
		let
			fun refill {yes, no} = case next t of
				NONE => false
				| SOME x => (
					if f x then yes := Fifo.push (!yes, x)
					else no := Fifo.push (!no, x)
					; true
				)
			fun force x = if refill x then force x else ()
			fun lengthYes (x as {yes, no}) () = (
				force x
				; Fifo.length (!yes)
			)
			fun nextYes (x as {yes, no}) () = case Fifo.pop (!yes) of
				NONE => if refill x then nextYes x () else NONE
				| SOME (new, x) => (
					yes := new
					; SOME x
				)
			fun cloneYes (x as {yes, no}) () =
				append (
					fromFifo (!yes)
					, filter f (clone t)
				)
			fun lengthNo (x as {yes, no}) () = (
				force x
				; Fifo.length (!no)
			)
			fun nextNo (x as {yes, no}) () = case Fifo.pop (!no) of
				NONE => if refill x then nextNo x () else NONE
				| SOME (new, x) => (
					no := new
					; SOME x
				)
			fun cloneNo (x as {yes, no}) () =
				append (
					fromFifo (!no)
					, filter (not o f) (clone t)
				)
			val x = {yes = ref Fifo.empty, no = ref Fifo.empty}
		in
			(
				create {
					length = lengthYes x
					, next = nextYes x
					, clone = cloneYes x
				}, create {
					length = lengthNo x
					, next = nextNo x
					, clone = cloneNo x
				}
			)
		end
	fun concat t =
		let
			fun length' {current, source} () =
				length (!current)
				+ fold (fn (u, sum) => length u + sum) 0 (clone source)
			fun next' (x as {current, source}) () = case next (!current) of
				NONE => (case next source of
					NONE => NONE
					| SOME y => (
						current := y
						; next' x ()
					)
				) | y => y
			fun clone' {current, source} () =
				let
					val x = {
						current = ref (clone (!current))
						, source = clone source
					}
				in
					create {
						length = length' x
						, next = next' x
						, clone = clone' x
					}
				end
			val x = {
				current = ref (empty ())
				, source = t
			}
		in
			create {
				length = length' x
				, next = next' x
				, clone = clone' x
			}
		end
	fun pairs t =
		create {
			length = fn () => length t div 2
			, next = fn () => (case next t of
				NONE => NONE
				| SOME x => (case next t of
					NONE => NONE
					| SOME y => SOME (x, y)
				)
			), clone = fn () => pairs (clone t)
		}
	fun find f t = case next t of
		NONE => NONE
		| x as (SOME y) =>
			if f y then x
			else find f t
	fun groups (t, n) =
		let
			fun length' () = (length t + 1) div n
			fun loop fifo =
				if Fifo.length fifo = n then SOME (fromFifo fifo)
				else case next t of
					NONE =>
						if Fifo.length fifo = 0 then NONE
						else SOME (fromFifo fifo)
					| SOME x => loop (Fifo.push (fifo, x))
			fun next' () = loop Fifo.empty
			fun clone' () = groups (clone t, n)
		in
			create {
				length = length'
				, next = next'
				, clone = clone'
			}
		end
	fun toList t =
		let
			fun loop l =
				case next t of
					NONE => rev l
					| SOME x => loop (x :: l)
		in
			loop nil
		end
	fun toGeneric tabulate t = tabulate (length t, fn _ => valOf (next t))
	fun toArray t = toGeneric Array.tabulate t
	val toCharArray = toGeneric CharArray.tabulate
	val toBoolArray = toGeneric BoolArray.tabulate
	val toIntArray = toGeneric IntArray.tabulate
	val toWordArray = toGeneric WordArray.tabulate
	val toRealArray = toGeneric RealArray.tabulate
	val toLargeIntArray = toGeneric LargeIntArray.tabulate
	val toLargeWordArray = toGeneric LargeWordArray.tabulate
	val toInt8Array = toGeneric Int8Array.tabulate
	val toInt16Array = toGeneric Int16Array.tabulate
	val toInt32Array = toGeneric Int32Array.tabulate
	val toInt64Array = toGeneric Int64Array.tabulate
	val toWord8Array = toGeneric Word8Array.tabulate
	val toWord16Array = toGeneric Word16Array.tabulate
	val toWord32Array = toGeneric Word32Array.tabulate
	val toWord64Array = toGeneric Word64Array.tabulate
	val toReal32Array = toGeneric Real32Array.tabulate
	val toReal64Array = toGeneric Real64Array.tabulate
	fun toVector t = toGeneric Vector.tabulate t
	val toCharVector = toGeneric CharVector.tabulate
	val toBoolVector = toGeneric BoolVector.tabulate
	val toIntVector = toGeneric IntVector.tabulate
	val toWordVector = toGeneric WordVector.tabulate
	val toRealVector = toGeneric RealVector.tabulate
	val toLargeIntVector = toGeneric LargeIntVector.tabulate
	val toLargeWordVector = toGeneric LargeWordVector.tabulate
	val toInt8Vector = toGeneric Int8Vector.tabulate
	val toInt16Vector = toGeneric Int16Vector.tabulate
	val toInt32Vector = toGeneric Int32Vector.tabulate
	val toInt64Vector = toGeneric Int64Vector.tabulate
	val toWord8Vector = toGeneric Word8Vector.tabulate
	val toWord16Vector = toGeneric Word16Vector.tabulate
	val toWord32Vector = toGeneric Word32Vector.tabulate
	val toWord64Vector = toGeneric Word64Vector.tabulate
	val toReal32Vector = toGeneric Real32Vector.tabulate
	val toReal64Vector = toGeneric Real64Vector.tabulate
	fun rev t =
		let
			fun length' {vector, position} () = !position + 1
			fun next' {vector, position} () =
				if !position >= 0 then
					SOME (Vector.sub (vector, !position))
					before position := !position - 1
				else NONE
			fun clone' {vector, position} () =
				let
					val x = {vector = vector, position = ref (!position)}
				in
					create {
						length = length' x
						, next = next' x
						, clone = clone' x
					}
				end
			val x = {vector = toVector t, position = ref (length t - 1)}
		in
			create {
				length = length' x
				, next = next' x
				, clone = clone' x
			}
		end
	fun exists f t = case next t of
		NONE => false
		| SOME x => if f x then true else exists f t
	fun all f t = case next t of
		NONE => true
		| SOME x => if f x then all f t else false
	fun unfold f a =
		let
			val a = ref a
			fun g () = case f (!a) of
				NONE => NONE
				| SOME (b, newA) => (
					a := newA
					; SOME b
				)
		in
			from g
		end
	fun tabulate (n, f) =
		let
			fun length' index () = n - !index
			fun next' index () =
				if !index = n then NONE
				else
					SOME (f (!index))
					before index := !index + 1
			fun clone' index () =
				let
					val index = ref (!index)
				in
					create {
						length = length' index
						, next = next' index
						, clone = clone' index
					}
				end
			val index = ref 0
		in
			create {
				length = length' index
				, next = next' index
				, clone = clone' index
			}
		end
	fun drop (t, n) =
		if n > 0 then case next t of
			NONE => ()
			| SOME _ => drop (t, n - 1)
		else ()
	fun take (t, n) =
		let
			fun loop fifo =
				if Fifo.length fifo = n then fromFifo fifo
				else case next t of
					NONE => fromFifo fifo
					| SOME x => loop (Fifo.push (fifo, x)
		in
			loop Fifo.empty
		end
	fun tokens f t =
		let
			fun force forceable =
				let
					fun loop (n, list, fifo) = case next t of
						NONE => forceable := Forced {
							length = ref n
							, contents = ref (rev l)
						} | SOME x => (case f x
						)
				in
				end
			fun length' forceable () =
			fun next' forceable () =
			fun clone' forceable () =
		in
		end
	fun fields f t
end
