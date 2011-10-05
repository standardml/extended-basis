val () = OS.Process.exit (
	case Shadow.length (Shadow.unfold (fn _ => NONE) 0) of
		0 => OS.Process.success
		| _ => OS.Process.failure
)
