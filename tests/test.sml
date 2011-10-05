val prefixes = case CommandLine.arguments () of
	nil => [""]
	| x => x
fun mlbs prefix =
	let
		val isMlb = String.isSuffix ".mlb"
		val hasPrefix = String.isPrefix prefix
		val stream = OS.FileSys.openDir "."
		fun loop l = case OS.FileSys.readDir stream of
			NONE => (
				OS.FileSys.closeDir stream
				; l
			) | SOME file =>
				if isMlb file andalso hasPrefix file then
					loop (file :: l)
				else loop l
	in
		loop nil
	end
fun sansMlb name = String.extract (name, 0, SOME (size name - size ".mlb"))
fun execute command = case command of
	nil => OS.Process.failure
	| arguments as (name :: _) => (case Posix.Process.fork () of
		SOME pid => (
			case Posix.Process.waitpid (
				Posix.Process.W_CHILD pid
				, [Posix.Process.W.flags nil]
			) of
				(_, Posix.Process.W_EXITED) => OS.Process.success
				| (_, Posix.Process.W_EXITSTATUS 0w0) => OS.Process.success
				| _ => OS.Process.failure
		) | NONE => Posix.Process.execp (name, arguments)
	)
local
	val sinceSaved = ref 0
in
	fun lastNewline string =
		let
			fun loop i =
				if i = ~1 then NONE
				else if String.sub (string, i) = #"\n" then SOME i
				else loop (i - 1)
		in
			loop (size string - 1)
		end
	fun print string = (
		case lastNewline string of
			NONE => sinceSaved := !sinceSaved + size string
			| SOME i => sinceSaved := size string - i - 1
		; TextIO.print string
	)
	fun save () = sinceSaved := 0
	fun reset () = (
		TextIO.print (
			CharVector.tabulate (
				!sinceSaved * 3
				, fn i => 
					if i mod 3 = 1 then #" "
					else #"\b"
			)
		); save ()
	)
end
fun executeTest mlb =
	let
		val base = sansMlb mlb
		val compile = {
			description = "compiling"
			, execute = fn () => OS.Process.isSuccess (execute ["mlton", mlb])
		}
		val run = {
			description = "running"
			, execute = fn () => OS.Process.isSuccess (execute ["./" ^ base])
		}
		val clean = {
			description = "cleaning up"
			, execute = fn () => (
				(
					OS.FileSys.remove base
					; true
				) handle _ => false
			)
		}
		fun executeLoop list = case list of
			nil => (
				print "finished.\n"
				; true
			) | {description, execute} :: next => (
				print (description ^ "...")
				;
					if execute () before reset () then executeLoop next
					else (
						print "failed.\n"
						; false
					)
			)
	in
		print (base ^ ": ")
		; save ()
		;
			if executeLoop [compile, run, clean] then ()
			else OS.Process.exit OS.Process.failure
	end
val () = app executeTest (List.concat (map mlbs prefixes))
