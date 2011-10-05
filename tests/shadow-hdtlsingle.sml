val () = OS.Process.exit (
	(
		ignore (Shadow.hd (Shadow.tl (Shadow.single 0)))
		; OS.Process.failure
	) handle Empty => OS.Process.success
)
