val () = OS.Process.exit (case Shadow.toList (Shadow.fromList [0, 1, 2]) of
	[0, 1, 2] => OS.Process.success
	| _ => OS.Process.failure
)
