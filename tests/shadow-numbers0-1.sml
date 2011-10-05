val () = OS.Process.exit (
	case Shadow.toList (Shadow.numbers {from = 0, to = ~1, step = ~1}) of
		[0, ~1] => OS.Process.success
		| _ => OS.Process.failure
)
