val () = OS.Process.exit (
	case Shadow.length (Shadow.fromList [0, 1]) of
		2 => OS.Process.success
		| _ => OS.Process.failure
)
