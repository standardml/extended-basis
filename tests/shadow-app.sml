val () = OS.Process.exit (
	let
		val array = Array.array (3, 0)
	in
		Shadow.app
			(fn i => Array.update (array, i, i))
			(Shadow.numbers {from = 0, to = 2, step = 1})
		; case Shadow.toList (Shadow.fromArray array) of
			[0, 1, 2] => OS.Process.success
			| _ => OS.Process.failure
	end
)
