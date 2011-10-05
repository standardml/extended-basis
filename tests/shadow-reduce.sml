val () = OS.Process.exit (
	case Shadow.reduce (op +) (Shadow.fromList [0, 1, 2]) of
		3 => OS.Process.success
		| _ => OS.Process.failure
)
