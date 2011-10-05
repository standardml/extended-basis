val () = OS.Process.exit (
	case Shadow.toString (Shadow.fromList [#"a", #"b", #"c"]) of
		"abc" => OS.Process.success
		| _ => OS.Process.failure
)
