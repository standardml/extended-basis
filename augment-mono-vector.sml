signature MONO_VECTOR = sig
	type t
	include CREATABLE
end
functor AugmentMonoVector (MonoVector: UNEXTENDED_MONO_VECTOR) :> MONO_VECTOR = struct
end
