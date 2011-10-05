val () = OS.Process.exit (case Shadow.toList (Shadow.fromList nil) of
	nil => OS.Process.success
	| _ => OS.Process.failure
)
