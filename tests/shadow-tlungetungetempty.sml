val () = OS.Process.exit (
	case
		Shadow.toList (
			Shadow.tl (Shadow.unget (Shadow.unget (Shadow.empty (), 0), 1))
		)
	of
		[0] => OS.Process.success
		| _ => OS.Process.failure
)
