val () = OS.Process.exit (
	case Shadow.get (Shadow.single 0) of
		SOME (0, _) => OS.Process.success
		| _ => OS.Process.failure
)
