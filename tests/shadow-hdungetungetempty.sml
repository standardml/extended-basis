val () = OS.Process.exit (
	case Shadow.hd (Shadow.unget (Shadow.unget (Shadow.empty (), 0), 1)) of
		1 => OS.Process.success
		| _ => OS.Process.failure
)
