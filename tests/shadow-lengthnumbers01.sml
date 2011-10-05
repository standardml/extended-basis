val () = OS.Process.exit (
	case Shadow.length (Shadow.numbers {from = 0, to = 1, step = 1}) of
		2 => OS.Process.success
		| _ => OS.Process.failure
)
