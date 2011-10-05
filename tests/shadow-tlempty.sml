val () = OS.Process.exit (
	(
		ignore (Shadow.tl (Shadow.empty ()))
		; OS.Process.failure
	) handle Empty => OS.Process.success
)
