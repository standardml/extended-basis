val () = OS.Process.exit (
	case Shadow.length (Shadow.fromList nil) of
		0 => OS.Process.success
		| _ => OS.Process.failure
)
