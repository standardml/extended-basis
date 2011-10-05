val () = OS.Process.exit (
	case Shadow.get (Shadow.fromList [0]) of
		SOME (0, x) => (case Shadow.get x of
			NONE => OS.Process.success
			| _ => OS.Process.failure
		) | _ => OS.Process.failure
)
