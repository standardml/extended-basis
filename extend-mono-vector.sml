(*
	ExtendMonoVector
	Functor for Extending a Basis Library Monomorphic Vector
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

functor ExtendMonoVector (
	structure MonoVector: UNEXTENDED_MONO_VECTOR
	structure MonoVectorSlice: UNEXTENDED_MONO_VECTOR_SLICE
	structure MonoArray: UNEXTENDED_MONO_ARRAY
	structure MonoArraySlice: UNEXTENDED_MONO_ARRAY_SLICE
	sharing type MonoVector.elem = MonoVectorSlice.elem
	sharing type MonoVector.elem = MonoArray.elem
	sharing type MonoVector.elem = MonoArraySlice.elem
) :> MONO_VECTOR = struct
	type t = MonoVector.vector
	type x = MonoVector.elem
	type sequence = t
	type pureSequence = t
	type element = x
	type pureElement = x
	type array = MonoArray.array
	type arraySlice = MonoArraySlice.slice
	type vectorSlice = MonoVectorSlice.slice
	type vector = t
	type elem = x
	structure Extra = MonoArrayOrVector (struct
		type t = vector
		type x = elem
		val length = MonoVector.length
		val sub = MonoVector.sub
		val tabulate = MonoVector.tabulate
		val foldl = MonoVector.foldl
		val foldr = MonoVector.foldr
		val concat = MonoVector.concat
	end)
	val isEmpty = Extra.isEmpty
	val length = MonoVector.length
	val hd = Extra.hd
	val sub = MonoVector.sub
	val appl = MonoVector.app
	val app = MonoVector.app
	val appli = MonoVector.appi
	val appi = MonoVector.appi
	val appr = Extra.appr
	val appri = Extra.appri
	val foldl = MonoVector.foldl
	val fold = MonoVector.foldl
	val foldli = MonoVector.foldli
	val foldi = MonoVector.foldli
	val foldr = MonoVector.foldr
	val foldri = MonoVector.foldri
	val reducel = Extra.reducel
	val reduce = Extra.reducel
	val reducer = Extra.reducer
	val findl = MonoVector.find
	val find = MonoVector.find
	val findli = MonoVector.findi
	val findi = MonoVector.findi
	val findr = Extra.findr
	val findri = Extra.findri
	val exists = MonoVector.exists
	val all = MonoVector.all
	val collate = MonoVector.collate
	val toShadow = Extra.toShadow
	val revToShadow = Extra.revToShadow
	val toList = Extra.toList
	val revToList = Extra.revToList
	val toArray = Extra.toTabulated MonoArray.tabulate
	val revToArray = Extra.revToTabulated MonoArray.tabulate
	val toPolyArray = Extra.toTabulated Array.tabulate
	val revToPolyArray = Extra.revToTabulated Array.tabulate
	fun toVector t = t
	val revToVector = Extra.revToTabulated MonoVector.tabulate
	val toPolyVector = Extra.toTabulated Vector.tabulate
	val revToPolyVector = Extra.revToTabulated Vector.tabulate
	val maxLen = MonoVector.maxLen
	val empty = Extra.empty
	val single = Extra.single
	val unfold = Extra.unfold
	val unfoldn = Extra.unfoldn
	val tabulate = MonoVector.tabulate
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
	val fromArray = Extra.fromLengthSub (MonoArray.length, MonoArray.sub)
	val fromRevArray = Extra.fromRevLengthSub (MonoArray.length, MonoArray.sub)
	val fromArraySlice = Extra.fromLengthSub (MonoArraySlice.length, MonoArraySlice.sub)
	val fromRevArraySlice = Extra.fromRevLengthSub (MonoArraySlice.length, MonoArraySlice.sub)
	val fromPolyArray = Extra.fromLengthSub (Array.length, Array.sub)
	val fromRevPolyArray = Extra.fromRevLengthSub (Array.length, Array.sub)
	val fromPolyArraySlice = Extra.fromLengthSub (ArraySlice.length, ArraySlice.sub)
	val fromRevPolyArraySlice = Extra.fromRevLengthSub (ArraySlice.length, ArraySlice.sub)
	fun fromVector v = v
	val fromRevVector = revToVector
	val fromVectorSlice = Extra.fromLengthSub (MonoVectorSlice.length, MonoVectorSlice.sub)
	val fromRevVectorSlice = Extra.fromRevLengthSub (MonoVectorSlice.length, MonoVectorSlice.sub)
	val fromPolyVector = Extra.fromLengthSub (Vector.length, Vector.sub)
	val fromRevPolyVector = Extra.fromRevLengthSub (Vector.length, Vector.sub)
	val fromPolyVectorSlice = Extra.fromLengthSub (VectorSlice.length, VectorSlice.sub)
	val fromRevPolyVectorSlice = Extra.fromRevLengthSub (VectorSlice.length, VectorSlice.sub)
	val fromList = MonoVector.fromList
	val map = MonoVector.map
	val mapi = MonoVector.mapi
	val getItem = Extra.getItem
	val ungetItem = Extra.ungetItem
	val tl = Extra.tl
	val append = Extra.append
	val filter = Extra.filter
	val mapPartial = Extra.mapPartial
	val partition = Extra.partition
	val rev = revToVector
	val concat = MonoVector.concat
	val concatWith = Extra.concatWith
	fun update (t, i, x) = tabulate (
		length t
		, fn j =>
			if j = i then x
			else sub (t, j)
	)
end
