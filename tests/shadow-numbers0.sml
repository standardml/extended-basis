val () = OS.Process.exit (
	case Shadow.toList (Shadow.numbers {from = 0, to = 0, step = 0}) of
		[0] => OS.Process.success
		| _ => OS.Process.failure
)
