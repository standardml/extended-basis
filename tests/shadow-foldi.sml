val () = OS.Process.exit (
	case Shadow.foldi (fn (i, a, b) => i + a + b) 0 (Shadow.fromList [0, 1, 2]) of
		6 => OS.Process.success
		| _ => OS.Process.failure
)
