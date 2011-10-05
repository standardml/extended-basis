val () = OS.Process.exit (
	case Shadow.isEmpty (Shadow.unget (Shadow.empty (), 0)) of
		false => OS.Process.success
		| _ => OS.Process.failure
)
