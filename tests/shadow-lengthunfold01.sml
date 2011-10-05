val () = OS.Process.exit (
	case Shadow.length (Shadow.unfold (fn 2 => NONE | i => SOME (i, i + 1)) 0) of
		2 => OS.Process.success
		| _ => OS.Process.failure
)
