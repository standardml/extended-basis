val () = OS.Process.exit (
	case Shadow.toList (Shadow.empty ()) of
		nil => OS.Process.success
		| _ => OS.Process.failure
)
