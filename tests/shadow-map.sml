val () = OS.Process.exit (
	case Shadow.toList (Shadow.map (fn x => x + 1) (Shadow.fromList [0, 1, 2])) of
		[1, 2, 3] => OS.Process.success
		| _ => OS.Process.failure
)
