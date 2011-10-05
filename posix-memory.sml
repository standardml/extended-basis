signature POSIX_MEMORY = sig
	datatype address = Fixed of MLton.Pointer.t | Hint of MLton.Pointer.t | Any
	structure PROT: sig
		include BIT_FLAGS
		val execute: flags
		val read: flags
		val write: flags
		val none: flags
	end
	datatype visibility = Shared | Private
	structure MAP: sig
		include BIT_FLAGS
		val align: flags
		val growsDown: flags
		val hasSemaphore: flags
		val hugeTLB: flags
		val initData: flags
		val locked: flags
		val noCache: flags
		val nonBlock: flags
		val noReserve: flags
		val populate: flags
		val stack: flags
		val text: flags
		val uninitialized: flags
	end
	datatype file = File of file_descr | Anonymous
	val mmap: address * int * PROT.flags * visibility * MAP.flags * file * Position.int
		-> MLton.Pointer.t option
	structure MREMAP: sig
		include BIT_FLAGS
		val mayMove: flags
		val fixed: flags
	end
	val mremap: MLton.Pointer.t * {old: int, new: int} * MREMAP.flags * MLton.Pointer.t option
		-> MLton.Pointer.t option
	val munmap: MLton.Pointer.t * int -> unit
end
structure Posix = struct open Posix structure Memory :> POSIX_MEMORY = struct
	structure MAP :> sig
		
	end = struct
	end
	local
		val primitiveMmap = _import "mmap":
			MLton.Pointer.t * LargeWord.word * Int32.int * Int32.int
			* Int32.int * LargeWord.word
			-> MLton.Pointer.t;
		val 
	in
		fun mmap {address, size, protection, visibility, flags, file, offset} =
			let
				val result = primitiveMmap (
					case address of
						Fixed p => p
						| Hint p => p
						| Any => MLton.Pointer.null
					, LargeWord.fromInt size
					, foldl
						(fn (sum, protection) => case protection of
							
				)
			in
			end
	end
	val primitiveMunmap = _import "munmap": MLton.Pointer.t * LargeWord.word -> int;
end end
