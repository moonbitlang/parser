///|
priv struct MoonfmtOptions {
  path : String
  overwrite : Bool
}

///|
fn moonfmt_command() -> @argparse.Command {
  Command(
    "moonfmt",
    about="Format a MoonBit source file.",
    flags=[
      FlagArg(
        "overwrite",
        short='w',
        long="overwrite",
        about="Overwrite the input file.",
      ),
    ],
    positionals=[
      PositionArg(
        "path",
        about="MoonBit source file to format.",
        num_args=@argparse.ValueRange::single(),
      ),
    ],
  )
}

///|
fn parse_moonfmt_args(args : ArrayView[String]) -> MoonfmtOptions raise {
  let matches = moonfmt_command().parse(argv=args, env=Map([]))
  let path = match matches.values.get("path") {
    Some([path]) => path
    _ => panic()
  }
  { path, overwrite: matches.flags.get("overwrite").unwrap_or(false) }
}

///|
fn moonfmt(args : ArrayView[String]) -> Unit {
  let options = parse_moonfmt_args(args) catch {
    e => {
      println(e)
      panic()
    }
  }
  let source = @fs.read_file_to_string(options.path) catch {
    e => {
      println(e)
      panic()
    }
  }
  let result = @fmt.format(source)
  if options.overwrite {
    @fs.write_string_to_file(options.path, result) catch {
      e => {
        println(e)
        panic()
      }
    }
  } else {
    println(result)
  }
}

///|
fn program_args() -> ArrayView[String] {
  let args = @env.args()
  if args.length() > 1 {
    args[1:]
  } else {
    []
  }
}

///|
fn main {
  moonfmt(program_args())
}

///|
test "parse moonfmt args" {
  let parsed = parse_moonfmt_args(["--overwrite", "input.mbt"]) catch {
    _ => panic()
  }
  inspect(parsed.path, content="input.mbt")
  inspect(parsed.overwrite, content="true")
  let short = parse_moonfmt_args(["-w", "input.mbt"]) catch { _ => panic() }
  inspect(short.path, content="input.mbt")
  inspect(short.overwrite, content="true")
}
