val () = OS.Process.exit (
	case
		Shadow.toList (
			Shadow.append (
				Shadow.fromList [0, 1, 2]
				, Shadow.fromList [3, 4, 5]
			)
		)
	of
		[0, 1, 2, 3, 4, 5] => OS.Process.success
		| _ => OS.Process.failure
)
