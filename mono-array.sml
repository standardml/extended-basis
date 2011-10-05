(*
	MonoArray
	Abstract Signature for Monomorphic Arrays
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
signature MONO_ARRAY = sig
	type t
	type x
	include CREATEABLE_MONO_SEQUENCE
		where type sequence = t
		where type element = x
	include IMPURE_MONO_SEQUENCE
		where type impureSequence = t
		where type impureElement = x
	type elem = x
end
