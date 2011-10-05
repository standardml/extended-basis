(*
	Functors for augmentation of simple vectors
*)
signature UNCREATABLE_VECTOR = sig
	type vector
	type elem
	val maxLen: int
	val length: vector -> int
	val sub: vector * int -> elem
	val appi: (int * elem -> unit) -> vector -> unit
	val app: (elem -> unit) -> vector -> unit
	val foldli: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
	val foldri: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
	val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a
	val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a
	val findi: (int * elem -> bool) -> vector -> (int * elem) option
	val find: (elem -> bool) -> vector -> elem option
	val exists: (elem -> bool) -> vector -> bool
	val all: (elem -> bool) -> vector -> bool
	val collate: (elem * elem -> order) -> vector * vector -> order
end
functor AugmentUncreatableVector (MonoVector: sig
	type vector
	type elem
	val maxLen: int
	val length: vector -> int
	val sub: vector * int -> elem
end) :> UNCREATABLE_VECTOR
	where type vector = MonoVector.vector
	where type elem = MonoVector.elem
= struct
	type vector = MonoVector.vector
	type elem = MonoVector.elem
	val maxLen = MonoVector.maxLen
	val length = MonoVector.length
	val sub = MonoVector.sub
	fun appi function vector =
		let
			val length = MonoVector.length vector
			fun loop index =
				if index >= length then ()
				else (
					function (index, MonoVector.sub (vector, index))
					; loop (index + 1)
				)
		in
			loop 0
		end
	fun app function vector = appi (fn (_, element) => function element) vector
	fun foldi step test function seed vector =
		let
			val length = MonoVector.length vector
			fun loop (index, state) =
				if test index then state
				else loop (
					step index
					, function (index, MonoVector.sub (vector, index), state)
				)
		in
			loop (0, seed)
		end
	fun foldli function seed vector = foldi
		(fn index => index + 1)
		(fn index => index >= MonoVector.length vector)
		function seed vector
	fun foldri function seed vector = foldi
		(fn index => index - 1)
		(fn index => index < 0)
		function seed vector
	fun foldl function seed vector =
		foldli (fn (_, element, state) => function (element, state)) seed vector
	fun foldr function seed vector =
		foldri (fn (_, element, state) => function (element, state)) seed vector
	fun findi function vector =
		let
			val length = MonoVector.length vector
			fun loop index =
				if index >= length then NONE
				else let
					val element = MonoVector.sub (vector, index)
				in
					if function (index, element) then SOME (index, element)
					else loop (index + 1)
				end
		in
			loop 0
		end
	fun find function vector =
		case findi (fn (_, element) => function element) vector of
			NONE => NONE
			| SOME (_, element) => SOME element
	fun exists function vector =
		case find function vector of
			NONE => false
			| SOME _ => true
	fun all function vector = not (exists (not o function) vector)
	fun collate compare (vectorA, vectorB) =
		let
			val lengthA = MonoVector.length vectorA
			val lengthB = MonoVector.length vectorB
			fun loop index =
				if index >= lengthA then (
					if index >= lengthB then EQUAL
					else LESS
				) else if index >= lengthB then GREATER
				else case
					compare (
						MonoVector.sub (vectorA, index)
						, MonoVector.sub (vectorB, index)
					)
				of
					EQUAL => loop (index + 1)
					| result => result
		in
			loop 0
		end
end
signature CREATABLE_VECTOR = sig
	include UNCREATABLE_VECTOR
	val fromList: elem list -> vector
	val tabulate: int * (int -> elem) -> vector
	val update: vector * int * elem -> vector
	val concat: vector list -> vector
	val mapi: (int * elem -> elem) -> vector -> vector
	val map: (elem -> elem) -> vector -> vector
end
functor AugmentCreatableVector (MonoVector: sig
	type vector
	type elem
	val maxLen: int
	val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> vector * 'a
	val length: vector -> int
	val sub: vector * int -> elem
end) :> CREATABLE_VECTOR
	where type vector = MonoVector.vector
	where type elem = MonoVector.elem
= struct
	structure Uncreatable = AugmentUncreatableVector (MonoVector)
	open Uncreatable
	fun fromList list =
		let
			val (vector, _) = MonoVector.unfoldi (
				List.length list
				, list
				, fn
					(_, nil) => raise Fail "impossible"
					| (_, head :: tail) => (head, tail)
			)
		in
			vector
		end
	fun tabulate (length, createElement) =
		let
			val (vector, _) = MonoVector.unfoldi (
				length
				, ()
				, fn (index, ()) => (createElement index, ())
			)
		in
			vector
		end
	fun update (oldVector, newIndex, newElement) =
		let
			val (newVector, _) = MonoVector.unfoldi (
				MonoVector.length oldVector
				, ()
				, fn (i, ()) =>
					if i = newIndex then (newElement, ())
					else (MonoVector.sub (oldVector, i), ())
			)
		in
			newVector
		end
	fun concat vectors =
		let
			val totalLength =
				List.foldl (fn (vector, length) =>
					MonoVector.length vector + length
				) 0 vectors
			fun loop x = case x of
				(_, (_, nil)) => raise Fail "impossible"
				| (totalIndex, (indexOfThis, both as (this :: rest))) =>
					if indexOfThis < MonoVector.length this then
						(
							MonoVector.sub (this, indexOfThis)
							, (indexOfThis + 1, both)
						)
					else loop (totalIndex, (0, rest))
			val (vector, _) = MonoVector.unfoldi (
				totalLength
				, (0, vectors)
				, loop
			)
		in
			vector
		end
	fun mapi function oldVector =
		let
			val (newVector, _) = MonoVector.unfoldi (
				MonoVector.length oldVector
				, ()
				, fn (index, ()) => (
						function (index, MonoVector.sub (oldVector, index))
						, ()
				)
			)
		in
			newVector
		end
	fun map function vector = mapi (fn (_, element) => function element) vector
end
functor CreatableFromUncreatableVector (MonoVector: sig
	type vector
	type elem
	val maxLen: int
	val length: vector -> int
	val sub: vector * int -> elem
end) :> MONO_VECTOR
	where type elem = MonoVector.elem
= AugmentCreatableVector (struct
	datatype vector =
		Uncreatable of MonoVector.vector
		| Creatable of MonoVector.elem Vector.vector
	type elem = MonoVector.elem
	val maxLen = Int.min (MonoVector.maxLen, Vector.maxLen)
	fun unfoldi (size, seed, step) =
		let
			val state = ref seed
		in
			(
				Creatable (Vector.tabulate (
					size
					, fn index =>
						let
							val (element, nextState) =
								step (index, !state)
						in
							state := nextState
							; element
						end
				))
				, !state
			)
		end
	fun length x = case x of
		Uncreatable vector => MonoVector.length vector
		| Creatable vector => Vector.length vector
	fun sub (x, index) = case x of
		Uncreatable vector => MonoVector.sub (vector, index)
		| Creatable vector => Vector.sub (vector, index)
end)
