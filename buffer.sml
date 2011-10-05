structure Buffer :> sig
	type t
	val create: unit -> t
	val length: t -> int
	val sub: t * int -> Word8.word
	val update: t * int * Word8.word -> unit
	val add: t * Word8.word -> unit
	val addVector: t * Word8Vector.vector -> unit
	val reader: t -> BinPrimIO.reader
	val writer: t -> BinPrimIO.writer
end = struct
	type t = {
		base: Word8Array.array ref
		, used: int ref
	}
	fun create () = {
		base = Word8Array.array (16, 0w0)
		, used = 0
	}
	fun length {base, used} = !used
	fun sub ({base, used}, i) =
		if i >= !used then raise Subscript
		else Word8Array.sub (!base, i)
	fun update ({base, used}, i, x) =
		if i >= !used then raise Subscript
		else Word8Array.update (!base, i, x)
	fun grow ({base, used}, needed) =
		let
			val growth = Int.max (
				Word8Array.length (!base) * 2
				, Word8Array.length (!base) + needed
			)
			val new = Word8Array.tabulate (
				Word8Array.length (!base) * 2
				, fn i =>
					if i < !used then Word8Array.sub (!base, i)
					else 0w0
			)
		in
			base := new
		end
	fun add (t as {base, used}, x) = (
		if Word8Array.length (!base) = !used then grow (t, 1) else ()
		; Word8Array.update (!base, !used, x)
		; used := !used + 1
	)
	fun addVector (t as {base, used}, v) = (
		if Word8Array.length (!base) < !used + Word8Vector.length v then
			grow (t, !used + Word8Vector.length v - Word8Array.length (!base))
		else ()
		; Word8Array.copyVec {src = v, dst = !base, di = !used}
		; used := !used + Word8Vector.length v
	)
	fun reader {base, used} =
		let
			datatype status = Open of int ref | Closed
			val status = ref (Open (ref 0))
			val name = "<buffer>"
			fun readVec requested = case !status of 
				Closed => raise IO.Io {
					name = name
					, function = "readVec"
					, cause = IO.ClosedStream
				} | Open pos =>
					let
						val toReturn = Int.max (
							requested
							, !used - !pos
						)
					in
						Word8ArraySlice.vector (
							Word8ArraySlice.slice (
								!base
								, !pos
								, SOME toReturn
							)
						) before pos := !pos + toReturn
					end
			fun readVecNB n = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "readVecNB"
					, cause = IO.ClosedStream
				} | Open _ => SOME (readVec n)
			fun readArr destination = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "readArr"
					, cause = IO.ClosedStream
				} | Open pos =>
					let
						val toReturn = Int.max (
							Word8ArraySlice.length destination
							, !used - !pos
						)
					in
						Word8ArraySlice.copy {
							src = Word8ArraySlice.slice (
								!base
								, !pos
								, SOME toReturn
							), dst = destination
							, di = 0
						}; toReturn
					end
			fun readArrNB s = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "readArrNB"
					, cause = IO.ClosedStream
				} | Open _ => SOME (readArr n)
			fun block () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "block"
					, cause = IO.ClosedStream
				} | Open _ => ()
			fun canInput () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "canInput"
					, cause = IO.ClosedStream
				} | Open _ => true
			fun avail () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "avail"
					, cause = IO.ClosedStream
				} | Open pos => SOME (!used - !pos)
			fun getPos () = case !status of
				(*
					The Basis Library specification says that
					raising this exception is not required for getPos.
					However, we have no useful information to return in
					this case.
				*)
				Closed => raise IO.Io {
					name = name
					, function = "getPos"
					, cause = IO.ClosedStream
				} | Open pos => Position.fromInt (!pos)
			fun setPos requested = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "setPos"
					, cause = IO.ClosedStream
				} | Open pos =>
					let
						val int = Position.toInt requested
					in
						if int > !used then raise Subscript
						else pos := int
					end
			fun endPos () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "endPos"
					, cause = IO.ClosedStream
				} | Open _ => Position.fromInt (!used)
			fun verifyPos () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "verifyPos"
					, cause = IO.ClosedStream
				} | Open pos => Position.fromInt (!pos)
			fun close () = status := Closed
		in
			BinPrimIO.RD {
				name = "<buffer>"
				, chunkSize = 1
				, readVec = SOME readVec
				, readArr = SOME readArr
				, readVecNB = SOME readVecNB
				, readArrNB = SOME readArrNB
				, block = SOME block
				, canInput = SOME canInput
				, avail = avail
				, getPos = SOME getPos
				, setPos = SOME setPos
				, endPos = SOME endPos
				, verifyPos = SOME verifyPos
				, close = close
				, ioDesc = NONE
			}
		end
	fun writer (t as {base, used}) =
		let
			datatype status = Closed | Open of int ref
			val status = ref (Open (ref 0))
			val name = "<buffer>"
			fun write (function, length, copy) slice = case !status of
				Closed => raise IO.Io {
					name = name
					, function = function
					, cause = IO.ClosedStream
				} | Open pos =>
					let
						val sliceLength = length slice
						val baseLength = Word8Array.length (!base)
					in
						if sliceLength + !pos > baseLength then
							grow (t, sliceLength + pos - baseLength)
						else ()
						; copy {
							src = slice
							, dst = Word8ArraySlice.full (!base)
							, di = !pos
						}; pos := !pos + sliceLength
						; if !pos > !used then used := !pos else ()
						; sliceLength
					end
			val writeVec = write (
				"writeVec"
				, Word8VectorSlice.length
				, Word8ArraySlice.copyVec
			)
			val writeArr = write (
				"writeArr"
				, Word8ArraySlice.length
				, Word8ArraySlice.copy
			)
			fun writeVecNB slice = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "writeVecNB"
					, cause = IO.ClosedStream
				} | Open _ => SOME (writeVec slice)
			fun writeArrNB slice = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "writeArrNB"
					, cause = IO.ClosedStream
				} | Open _ => SOME (writeArr slice)
			fun block () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "block"
					, cause = IO.ClosedStream
				} | Open _ => ()
			fun canOutput () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "canOutput"
					, cause = IO.ClosedStream
				} | Open _ => true
			fun getPos () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "getPos"
					, cause = IO.ClosedStream
				} | Open pos => Position.fromInt (!pos)
			fun setPos requested = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "setPos"
					, cause = IO.ClosedStream
				} | Open pos =>
					let
						val baseLength = Word8Array.length (!base)
						val requested = Position.toInt requested
					in
						if requested > baseLength then
							grow (t, requested - baseLength)
						else ()
						; pos := requested
					end
			fun endPos () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "endPos"
					, cause = IO.ClosedStream
				} | Open _ => !used
			fun varifyPos () = case !status of
				Closed => raise IO.Io {
					name = name
					, function = "verifyPos"
					, cause = IO.ClosedStream
				} | Open pos => Position.fromInt (!pos)
			fun close () = status := Closed
		in
			BinPrimIO.WR {
				name = name
				, chunkSize = 1
				, writeVec = SOME writeVec
				, writeArr = SOME writeArr
				, writeVecNB = SOME writeVecNB
				, writeArrNB = SOME writeArrNB
				, block = SOME block
				, canOutput = SOME canOutput
				, getPos = SOME getPos
				, setPos = SOME setPos
				, endPos = SOME endPos
				, verifyPos = SOME verifyPos
				, close = close
				, ioDesc = NONE
			}
		end
end
