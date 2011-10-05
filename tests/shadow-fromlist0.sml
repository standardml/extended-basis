val () = OS.Process.exit (case Shadow.toList (Shadow.fromList [0]) of
	[0] => OS.Process.success
	| _ => OS.Process.failure
)
