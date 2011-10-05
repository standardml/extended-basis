val () = OS.Process.exit (
	case Shadow.toList (Shadow.numbers {from = 0, to = 3, step = 2}) of
		[0, 2] => OS.Process.success
		| _ => OS.Process.failure
)
