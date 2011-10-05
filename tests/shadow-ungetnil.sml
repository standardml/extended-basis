val () = OS.Process.exit (
	case Shadow.toList (Shadow.unget (Shadow.fromList nil, 0)) of
		[0] => OS.Process.success
		| _ => OS.Process.failure
)
