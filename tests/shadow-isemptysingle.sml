val () = OS.Process.exit (
	case Shadow.isEmpty (Shadow.single 0) of
		false => OS.Process.success
		| _ => OS.Process.failure
)
