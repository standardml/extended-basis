val () = OS.Process.exit (
	(
		ignore (Shadow.length (Shadow.numbers {from = 0, to = 1, step = 0}))
		; OS.Process.failure
	) handle Shadow.InfiniteLength => OS.Process.success
)
