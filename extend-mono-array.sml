(*
	ExtendMonoArray
	Functor for Extending a Basis Library Monomorphic Array
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
functor ExtendMonoArray (
	structure MonoVector: UNEXTENDED_MONO_VECTOR
	structure MonoVectorSlice: UNEXTENDED_MONO_VECTOR_SLICE
	structure MonoArray: UNEXTENDED_MONO_ARRAY
	structure MonoArraySlice: UNEXTENDED_MONO_ARRAY_SLICE
	sharing type MonoVector.elem = MonoVectorSlice.elem
	sharing type MonoVector.elem = MonoArray.elem
	sharing type MonoVector.elem = MonoArraySlice.elem
) :> MONO_ARRAY = struct
	type t = MonoArray.array
	type x = MonoArray.elem
	type sequence = t
	type element = x
	type elem = x
	type array = t
	type slice = MonoArraySlice.slice
	type arraySlice = MonoArraySlice.slice
	type vector = MonoVector.vector
	type vectorSlice = MonoVectorSlice.slice
	type impureArray = array
	type impureElement = element
	type impureArraySlice = arraySlice
	type impureSequence = sequence
	type impureVector = vector
	type impureVectorSlice = vectorSlice
	fun concat l =
		let
			val l = ref l
			val j = ref 0
		in
			MonoArray.tabulate (
				List.foldl (fn (a, b) => MonoArray.length a + b) 0 (!l)
				, fn _ =>
					let
						fun loop () = case !l of
							nil => raise Fail "impossible"
							| x :: y =>
								if !j >= MonoArray.length x then (
									l := y
									; j := 0
									; loop ()
								) else (
									MonoArray.sub (x, !j)
									before j := !j + 1
								)
					in
						loop ()
					end
			)
		end
	structure Extra = MonoArrayOrVector (struct
		type t = t
		type x = x
		val length = MonoArray.length
		val sub = MonoArray.sub
		val tabulate = MonoArray.tabulate
		val foldl = MonoArray.foldl
		val foldr = MonoArray.foldr
		val concat = concat
	end)
	val isEmpty = Extra.isEmpty
	val length = MonoArray.length
	val hd = Extra.hd
	val sub = MonoArray.sub
	val appl = MonoArray.app
	val app = MonoArray.app
	val appli = MonoArray.appi
	val appi = MonoArray.appi
	val appr = Extra.appr
	val appri = Extra.appri
	val foldl = MonoArray.foldl
	val fold = MonoArray.foldl
	val foldli = MonoArray.foldli
	val foldi = MonoArray.foldli
	val foldr = MonoArray.foldr
	val foldri = MonoArray.foldri
	val reducel = Extra.reducel
	val reduce = Extra.reducel
	val reducer = Extra.reducer
	val findl = MonoArray.find
	val find = MonoArray.find
	val findli = MonoArray.findi
	val findi = MonoArray.findi
	val findr = Extra.findr
	val findri = Extra.findri
	val exists = MonoArray.exists
	val all = MonoArray.all
	val collate = MonoArray.collate
	val toShadow = Extra.toShadow
	val revToShadow = Extra.revToShadow
	val toList = Extra.toList
	val revToList = Extra.revToList
	fun toArray x = x
	val revToArray = Extra.revToTabulated MonoArray.tabulate
	val toPolyArray = Extra.toTabulated Array.tabulate
	val revToPolyArray = Extra.revToTabulated Array.tabulate
	val toVector = Extra.toTabulated MonoVector.tabulate
	val revToVector = Extra.revToTabulated MonoVector.tabulate
	val toPolyVector = Extra.toTabulated Vector.tabulate
	val revToPolyVector = Extra.revToTabulated Vector.tabulate
	val maxLen = MonoArray.maxLen
	val empty = Extra.empty
	val single = Extra.single
	val unfold = Extra.unfold
	val unfoldn = Extra.unfoldn
	val tabulate = MonoArray.tabulate
	val split = Extra.split
	val splitAt = Extra.splitAt
	val drop = Extra.drop
	val trim = Extra.trim
	val limit = Extra.limit
	val take = Extra.take
	val tokens = Extra.tokens
	val fields = Extra.fields
	val translate = Extra.translate
	val extract = Extra.extract
	val insert = Extra.insert
	val delete = Extra.delete
	val fromShadow = Extra.fromShadow
	val fromRevShadow = Extra.fromRevShadow
	fun fromArray x = x
	val fromRevArray = revToArray
	val fromArraySlice = Extra.fromLengthSub (MonoArraySlice.length, MonoArraySlice.sub)
	val fromRevArraySlice = Extra.fromRevLengthSub (MonoArraySlice.length, MonoArraySlice.sub)
	val fromPolyArray = Extra.fromLengthSub (Array.length, Array.sub)
	val fromRevPolyArray = Extra.fromRevLengthSub (Array.length, Array.sub)
	val fromPolyArraySlice = Extra.fromLengthSub (ArraySlice.length, ArraySlice.sub)
	val fromRevPolyArraySlice = Extra.fromRevLengthSub (ArraySlice.length, ArraySlice.sub)
	val fromVector = Extra.fromLengthSub (MonoVector.length, MonoVector.sub)
	val fromRevVector = Extra.fromRevLengthSub (MonoVector.length, MonoVector.sub)
	val fromVectorSlice = Extra.fromLengthSub (MonoVectorSlice.length, MonoVectorSlice.sub)
	val fromRevVectorSlice = Extra.fromRevLengthSub (MonoVectorSlice.length, MonoVectorSlice.sub)
	val fromPolyVector = Extra.fromLengthSub (Vector.length, Vector.sub)
	val fromRevPolyVector = Extra.fromRevLengthSub (Vector.length, Vector.sub)
	val fromPolyVectorSlice = Extra.fromLengthSub (VectorSlice.length, VectorSlice.sub)
	val fromRevPolyVectorSlice = Extra.fromLengthSub (VectorSlice.length, VectorSlice.sub)
	val fromList = MonoArray.fromList
	fun map f a = MonoArray.tabulate (MonoArray.length a, fn i => f (MonoArray.sub (a, i)))
	fun mapi f a = MonoArray.tabulate (MonoArray.length a, fn i => f (i, MonoArray.sub (a, i)))
	val getItem = Extra.getItem
	val ungetItem = Extra.ungetItem
	val tl = Extra.tl
	val append = Extra.append
	val filter = Extra.filter
	val mapPartial = Extra.mapPartial
	val partition = Extra.partition
	val rev = revToArray
	val concatWith = Extra.concatWith
	val update = MonoArray.update
	val modify = MonoArray.modify
	val modifyi = MonoArray.modifyi
	val copy = MonoArray.copy
	fun copyAppli appli {src, dst, di} = appli (fn (i, x) => update (dst, di + i, x)) src
	fun copyRevLengthSub (length, sub) {src, dst, di} =
		let
			val n = length src
			fun loop i =
				if i >= n then ()
				else (
					update (dst, di + (n - 1 - i), sub (src, i))
					; loop (i + 1)
				)
		in
			loop 0
		end
	val copyShadow = copyAppli Shadow.appli
	fun copyRevShadow {src, dst, di} = copyShadow {src = Shadow.rev src, dst = dst, di = di}
	val copyArray = MonoArray.copy
	val copyRevArray = copyRevLengthSub (MonoArray.length, MonoArray.sub)
	val copyArraySlice = copyAppli MonoArraySlice.appi
	val copyRevArraySlice = copyRevLengthSub (MonoArraySlice.length, MonoArraySlice.sub)
	val copyPolyArray = copyAppli Array.appi
	val copyRevPolyArray = copyRevLengthSub (Array.length, Array.sub)
	val copyPolyArraySlice = copyAppli ArraySlice.appi
	val copyRevPolyArraySlice = copyRevLengthSub (ArraySlice.length, ArraySlice.sub)
	val copyVector = copyAppli MonoVector.appi
	val copyVec = copyVector
	val copyRevVector = copyRevLengthSub (MonoVector.length, MonoVector.sub)
	val copyVectorSlice = copyAppli MonoVectorSlice.appi
	val copyRevVectorSlice = copyRevLengthSub (MonoVectorSlice.length, MonoVectorSlice.sub)
	val copyPolyVector = copyAppli Vector.appi
	val copyRevPolyVector = copyRevLengthSub (Vector.length, Vector.sub)
	val copyPolyVectorSlice = copyAppli VectorSlice.appi
	val copyRevPolyVectorSlice = copyRevLengthSub (VectorSlice.length, VectorSlice.sub)
	val copyList = copyAppli (fn f => fn l => 
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
	)
end
