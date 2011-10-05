(*
	MonoSequence
	Abstract Signatures for Integer-Addressable Monomorphic Collections
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
signature MONO_SEQUENCE = sig
	type sequence
	type element
	type array
	type arraySlice
	type vector
	type vectorSlice

	(*
		isEmpty t returns true if t is empty, and false if
		t is not empty.
	*)
	val isEmpty: sequence -> bool

	(*
		length t returns the number of elements in t.
		If t does not have a finite length, then it raises
		InfiniteLength.
	*)
	val length: sequence -> int

	(*
		hd t returns the element at the head of the sequence.
		It raises Empty if t is empty.
	*)
	val hd: sequence -> element

	(*
		sub (t, i) returns the ith element of t. It
		raises Subscript if the end of the sequence is
		reached before the ith element.
	*)
	val sub: sequence * int -> element

	(*
		appl f t applies f to every element of t, from
		the first to the last element.
		app is equivalent to appl.
	*)
	val appl: (element -> unit) -> sequence -> unit
	val app: (element -> unit) -> sequence -> unit

	(*
		appli f t applies f to a pair of the index of each
		element of t, along with the element itself, from the
		first to the last.
		appi is equivalent to appli.
	*)
	val appli: (int * element -> unit) -> sequence -> unit
	val appi: (int * element -> unit) -> sequence -> unit

	(*
		appr f t applies f to every element of t, from
		the last to the first element.
	*)
	val appr: (element -> unit) -> sequence -> unit

	(*
		appri f t applies f to a pair of the index of each
		element of t, along with the element itself, from the
		last to the first.
	*)
	val appri: (int * element -> unit) -> sequence -> unit

	(*
		foldl f b t returns f (xn, ... f (x1, f (x0, b)) ...),
		where x0, x1, ... xn are the elements of t, or simply b
		if the sequence is empty.
		fold is equivalent to foldl.
	*)
	val foldl: (element * 'a -> 'a) -> 'a -> sequence -> 'a
	val fold: (element * 'a -> 'a) -> 'a -> sequence -> 'a

	(*
		foldli f b t returns (f (n, xn, ... f (1, x1, f (0, x0, b)) ...),
		where x0, x1, ... xn are the elements of t, or simply b
		if the sequence is empty.
		foldi is equivalent to foldli.
	*)
	val foldli: (int * element * 'a -> 'a) -> 'a -> sequence -> 'a
	val foldi: (int * element * 'a -> 'a) -> 'a -> sequence -> 'a

	val foldr: (element * 'a -> 'a) -> 'a -> sequence -> 'a
	val foldri: (int * element * 'a -> 'a) -> 'a -> sequence -> 'a

	(*
		reducel f t returns f (xn, ... f (x2, f (x1, x0)) ...),
		where x0, x1, x2, ... xn are the elements of t, or if t
		contains a single element, simply returns that element.
		If t is empty, it raises Empty.
		reduce is equivalent to reducel.
	*)
	val reducel: (element * element -> element) -> sequence -> element
	val reduce: (element * element -> element) -> sequence -> element

	(*
		reducer f t returns f (x0, f (x1, ... f (xn-1, xn) ...)),
		where x0, x1, ... xn-1, xn are the elements of t, of if t
		contains a single element, simply returns that element.
		If t is empty it raises Empty.
	*)
	val reducer: (element * element -> element) -> sequence -> element

	(*
		findl f t returns SOME x, in which x is the
		first element which, applied to f, results in true.
		If no element results in true, it returns NONE.
		find is equivalent to findl.
	*)
	val findl: (element -> bool) -> sequence -> element option
	val find: (element -> bool) -> sequence -> element option

	(*
		findli f t returns SOME (i, x), in which i is the
		index of the first element which, applied to f, results
		in true, and x is the element itself. If no element
		results in true, it returns NONE.
		findi is equivalent to findli.
	*)
	val findli: (int * element -> bool) -> sequence -> (int * element) option
	val findi: (int * element -> bool) -> sequence -> (int * element) option

	(*
		findr f t returns SOME x, in which x is the
		last element which, applied to f, results in true.
		If no element results in true, it returns NONE.
	*)
	val findr: (element -> bool) -> sequence -> element option

	(*
		findri f t returns SOME (i, x), in which i is the
		index of the last element which, applied to f, results
		in true, and x is the element itself. If no element
		results in true, it returns NONE.
	*)
	val findri: (int * element -> bool) -> sequence -> (int * element) option

	(*
		exists f t returns true on the first element which, when
		applied to f, yields true, and returns false if no such
		element is found.
	*)
	val exists: (element -> bool) -> sequence -> bool

	(*
		all f t returns false on the first element which, when
		applied to f, yields false, and returns true if no such
		element is found.
	*)
	val all: (element -> bool) -> sequence -> bool

	(*
		collate f (t, u) compares t and u lexicographically,
		using f to compare elements of each, and returns LESS if
		t is less than u, EQUAL if t is equal to u, and GREATER
		if t is greater than u.
	*)
	val collate: (element * element -> order) -> sequence * sequence -> order

	(*
		Convert to another sequence type. The functions with rev
		reverse before conversion.
	*)
	val toShadow: sequence -> element Shadow.t
	val revToShadow: sequence -> element Shadow.t
	val toList: sequence -> element list
	val revToList: sequence -> element list
	val toArray: sequence -> array
	val revToArray: sequence -> array
	val toPolyArray: sequence -> element Array.array
	val revToPolyArray: sequence -> element Array.array
	val toVector: sequence -> vector
	val revToVector: sequence -> vector
	val toPolyVector: sequence -> element Vector.vector
	val revToPolyVector: sequence -> element Vector.vector
end

signature CREATEABLE_MONO_SEQUENCE = sig
	include MONO_SEQUENCE
	val maxLen: int

	(*
		empty () returns an empty sequence.
	*)
	val empty: unit -> sequence

	(*
		single x returns a sequenge which contains a single
		element of x.
	*)
	val single: element -> sequence

	(*
		unfold f a0 creates a sequence b0, b1, ... bn, through
		a process where f a0 yields SOME (b0, a1), f a1 yields
		SOME (b1, a2), etc. until f an yields SOME (bn, an+1)
		and f an+1 yields NONE.
	*)
	val unfold: ('a -> (element * 'a) option) -> 'a -> sequence

	(*
		unfoldn f (n, a0) creates a sequence b0, b1, ... bn,
		through a process where f a0 yields (b0, a1), f a1
		yields (b1, a2), etc. until the sequence reaches length
		n.
	*)
	val unfoldn: ('a -> element * 'a) -> int * 'a -> sequence

	(*
		tabulate (n, f) creates a sequence of length n, where
		the elements are f 0, f 1, ... f (n - 1).
	*)
	val tabulate: int * (int -> element) -> sequence

	(*
		split f t returns two subsequences: the first consists
		all the elements from at the beginning of t
		that satisfy the predicate f, and the second consists of
		the remaining elements.
	*)
	val split: (element -> bool) -> sequence -> sequence * sequence

	(*
		splitAt (t, n) returns two subsequences: the first consists
		of the first n elements of t, and the second consists of
		the remaining elements.
	*)
	val splitAt: sequence * int -> sequence * sequence

	(*
		drop f t returns the sequence remaining after dropping the
		elements from the beginning of t that, when applied
		to f, return true.
	*)
	val drop: (element -> bool) -> sequence -> sequence

	(*
		trim (t, n) returns the sequence remaining after trimming
		n elements from the beginning of t.
	*)
	val trim: sequence * int -> sequence

	(*
		limit (t, n) returns a sequence limited to the first
		n elements of t.
	*)
	val limit: sequence * int -> sequence

	(*
		take f t returns a sequence consisting of the elements
		from the beginning of t that, when applied to f, yield
		true.
	*)
	val take: (element -> bool) -> sequence -> sequence

	(*
		tokens f t returns a list of subsequences separated
		by elements that, when applied to f, yield true. Multiple
		adjacent separating elements are treated as a single separator,
		thus there are no empty subsequences in the result.
	*)
	val tokens: (element -> bool) -> sequence -> sequence list

	(*
		fields f t returns a list of subsequences separated
		by elements that, when applied to f, yield true. Multiple
		adjacent separating elements are treated as separating
		empty subsequences, contrary to tokens above.
	*)
	val fields: (element -> bool) -> sequence -> sequence list

	(*
		translate f t returns a sequence of the concatenation
		of the results of f applied to each element of t.
	*)
	val translate: (element -> sequence) -> sequence -> sequence

	(*
		extract (t, i, NONE) returns a subsequence of t,
		starting at the ith element and continuing to the end.
		extract (t, i, SOME n) returns a subsequence of t,
		starting at the ith element and continuing for n elements.
	*)
	val extract: sequence * int * int option -> sequence

	(*
		insert (t, i, x) returns a new sequence, with
		x inserted at position i. The elements from
		position i to the end of the sequence are
		shifted to the right.
	*)
	val insert: sequence * int * element -> sequence

	(*
		delete (t, i) returns a new sequence, with the
		the ith element in t removed.
	*)
	val delete: sequence * int -> sequence

	(*
		These convert from another data structure to this
		type of sequence. The ones with Rev in the name
		reverse while converting.
	*)
	val fromShadow: element Shadow.t -> sequence
	val fromRevShadow: element Shadow.t -> sequence
	val fromArray: array -> sequence
	val fromRevArray: array -> sequence
	val fromArraySlice: arraySlice -> sequence
	val fromRevArraySlice: arraySlice -> sequence
	val fromPolyArray: element Array.array -> sequence
	val fromRevPolyArray: element Array.array -> sequence
	val fromPolyArraySlice: element ArraySlice.slice -> sequence
	val fromRevPolyArraySlice: element ArraySlice.slice -> sequence
	val fromVector: vector -> sequence
	val fromRevVector: vector -> sequence
	val fromVectorSlice: vectorSlice -> sequence
	val fromRevVectorSlice: vectorSlice -> sequence
	val fromPolyVector: element Vector.vector -> sequence
	val fromRevPolyVector: element Vector.vector -> sequence
	val fromPolyVectorSlice: element VectorSlice.slice -> sequence
	val fromRevPolyVectorSlice: element VectorSlice.slice -> sequence
	val fromList: element list -> sequence

	(*
		map f t returns a sequence of the results of applying f to
		each element of t.
	*)
	val map: (element -> element) -> sequence -> sequence

	(*
		mapi f t returns a sequence of the results of applying f
		to a pair of the index of each element of t, along with the element
		itself.
	*)
	val mapi: (int * element -> element) -> sequence -> sequence

	(*
		getItem t returns NONE if t is empty, or SOME (x, u)
		in which x is the next element and u is a sequence
		containing the remaining elements.
	*)
	val getItem: sequence -> (element * sequence) option

	(*
		ungetItem (t, x) returns a sequence in which x is
		the first element, and t makes up the remaining
		elements.
	*)
	val ungetItem: sequence * element -> sequence

	(*
		tl t returns the sequence consisting of all but the
		first element of t. It raises Empty if t is empty.
	*)
	val tl: sequence -> sequence

	(*
		append (t, u) returns a sequence of the elements
		of t, followed by the elements of u.
	*)
	val append: sequence * sequence -> sequence

	(*
		filter f t returns a sequence of only the elements
		of t in which f returns true.
	*)
	val filter: (element -> bool) -> sequence -> sequence

	(*
		mapPartial f t returns a sequence of only the elements
		where the application of f returns SOME, with the SOME
		stripped out.
	*)
	val mapPartial: (element -> element option) -> sequence -> sequence

	(*
		partition f t returns a pair of sequences. The first
		sequence in the pair consists of elements which, when
		applied to f, yield true, and the second sequence
		consists of the the elements which, when applied to f,
		yield false.
	*)
	val partition: (element -> bool) -> sequence -> sequence * sequence

	(*
		rev t returns a sequence with the elements of t
		in reverse order.
	*)
	val rev: sequence -> sequence

	(*
		concat l returns a sequence that is a concatentation of
		all the sequences in l.
	*)
	val concat: sequence list -> sequence

	(*
		concatWith t l returns a sequence that is a concatentation
		of all the sequences in l, with t inserted in between them.
	*)
	val concatWith: sequence -> sequence list -> sequence
end

signature PURE_MONO_SEQUENCE = sig
	type pureSequence
	type pureElement

	(*
		update (t, i, x) returns a new sequence, with
		the ith element in t replaced by x.
	*)
	val update: pureSequence * int * pureElement -> pureSequence
end

signature IMPURE_MONO_SEQUENCE = sig
	type impureSequence
	type impureElement
	type impureVector
	type impureVectorSlice
	type impureArray
	type impureArraySlice
	val update: impureSequence * int * impureElement -> unit
	val modify: (impureElement -> impureElement) -> impureSequence -> unit
	val modifyi: (int * impureElement -> impureElement) -> impureSequence -> unit
	val copy: {src: impureSequence, dst: impureSequence, di: int} -> unit
	val copyShadow: {src: impureElement Shadow.t, dst: impureSequence, di: int} -> unit
	val copyRevShadow: {src: impureElement Shadow.t, dst: impureSequence, di: int} -> unit
	val copyArray: {src: impureArray, dst: impureSequence, di: int} -> unit
	val copyRevArray: {src: impureArray, dst: impureSequence, di: int} -> unit
	val copyArraySlice: {src: impureArraySlice, dst: impureSequence, di: int} -> unit
	val copyRevArraySlice: {src: impureArraySlice, dst: impureSequence, di: int} -> unit
	val copyPolyArray: {src: impureElement Array.array, dst: impureSequence, di: int} -> unit
	val copyRevPolyArray: {src: impureElement Array.array, dst: impureSequence, di: int} -> unit
	val copyPolyArraySlice: {src: impureElement ArraySlice.slice, dst: impureSequence, di: int}
		-> unit
	val copyRevPolyArraySlice: {
		src: impureElement ArraySlice.slice
		, dst: impureSequence, di: int
	} -> unit
	val copyVector: {src: impureVector, dst: impureSequence, di: int} -> unit
	val copyVec: {src: impureVector, dst: impureSequence, di: int} -> unit
	val copyRevVector: {src: impureVector, dst: impureSequence, di: int} -> unit
	val copyVectorSlice: {src: impureVectorSlice, dst: impureSequence, di: int} -> unit
	val copyRevVectorSlice: {src: impureVectorSlice, dst: impureSequence, di: int} -> unit
	val copyPolyVector: {src: impureElement Vector.vector, dst: impureSequence, di: int} -> unit
	val copyRevPolyVector: {src: impureElement Vector.vector, dst: impureSequence, di: int}
		-> unit
	val copyPolyVectorSlice: {src: impureElement VectorSlice.slice, dst: impureSequence, di: int}
		-> unit
	val copyRevPolyVectorSlice: {
		src: impureElement VectorSlice.slice
		, dst: impureSequence
		, di: int
	} -> unit
	val copyList: {src: impureElement list, dst: impureSequence, di: int} -> unit
end
