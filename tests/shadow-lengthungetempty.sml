val () = OS.Process.exit (
	case Shadow.length (Shadow.unget (Shadow.empty (), 0)) of
		1 => OS.Process.success
		| _ => OS.Process.failure
)
