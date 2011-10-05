
val () = OS.Process.exit (case Shadow.toList (Shadow.fromList [0, 1]) of
	[0, 1] => OS.Process.success
	| _ => OS.Process.failure
)
