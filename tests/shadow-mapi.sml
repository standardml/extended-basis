val () = OS.Process.exit (
	case Shadow.toList (Shadow.mapi (fn (i, x) => i + x) (Shadow.fromList [0, 1, 2])) of
		[0, 2, 4] => OS.Process.success
		| _ => OS.Process.failure
)
