val () = OS.Process.exit (
	case Shadow.isEmpty (Shadow.empty ()) of
		true => OS.Process.success
		| _ => OS.Process.failure
)
