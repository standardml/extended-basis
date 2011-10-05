val () = OS.Process.exit (case Shadow.get (Shadow.fromList nil) of
	NONE => OS.Process.success
	| _ => OS.Process.failure
)
