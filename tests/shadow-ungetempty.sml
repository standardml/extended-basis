val () = OS.Process.exit (
	case Shadow.toList (Shadow.unget (Shadow.empty (), 0)) of
		[0] => OS.Process.success
		| _ => OS.Process.failure
)
