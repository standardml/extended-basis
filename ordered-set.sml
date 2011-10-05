(*
	Ordered Sets
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

signature ORDERED_SET = sig
	structure Element: ORDERED
	type t
	val isEmpty: t -> bool
	val length: t -> int
	val empty: t
	val single: Element.t -> t
	val put: t * Element.t -> t
	val remove: t * Element.t -> t
	val member: t * Element.t -> bool
	val getFirst: t -> (Element.t * t) option
	val first: t -> Element.t
	val getLast: t -> (Element.t * t) option
	val last: t -> Element.t
end
