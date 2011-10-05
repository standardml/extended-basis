val () = OS.Process.exit (
	case Shadow.toList (Shadow.unfold (fn 2 => NONE | i => SOME (i, i + 1)) 0) of
		[0, 1] => OS.Process.success
		| _ => OS.Process.failure
)
