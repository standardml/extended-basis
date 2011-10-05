val () = OS.Process.exit (
	case Shadow.toList (Shadow.unfold (fn _ => NONE) 0) of
		nil => OS.Process.success
		| _ => OS.Process.failure
)
