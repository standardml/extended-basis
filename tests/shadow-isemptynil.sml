val () = OS.Process.exit (
	case Shadow.isEmpty (Shadow.fromList nil) of
		true => OS.Process.success
		| _ => OS.Process.failure
)
