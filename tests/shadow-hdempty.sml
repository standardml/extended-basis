val () = OS.Process.exit (
	(
		ignore (Shadow.hd (Shadow.empty ()))
		; OS.Process.failure
	) handle Empty => OS.Process.success
)
