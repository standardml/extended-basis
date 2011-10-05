val () = OS.Process.exit (
	case Shadow.toList (
		Shadow.fromStream 
			TextIO.StreamIO.input1
			(TextIO.getInstream (TextIO.openString "012"))
	) of
		[#"0", #"1", #"2"] => OS.Process.success
		| _ => OS.Process.failure
)
