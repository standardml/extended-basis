val () = OS.Process.exit (
	case Shadow.length (Shadow.empty ()) of
		0 => OS.Process.success
		| _ => OS.Process.failure
)
