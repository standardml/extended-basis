val () = OS.Process.exit (
	case Shadow.toList (
		Shadow.for {start = 0, test = fn n => n < 3, step = fn n => n + 1}
	) of
		[0, 1, 2] => OS.Process.success
		| _ => OS.Process.failure
)
