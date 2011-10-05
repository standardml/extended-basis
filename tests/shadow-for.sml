val () = OS.Process.exit (
	case Shadow.toList (
		Shadow.for {start = (), test = fn () => false, step = fn () => ()}
	) of
		nil => OS.Process.success
		| _ => OS.Process.failure
)
