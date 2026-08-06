///|
priv struct MoonfmtOptions {
  path : String
  overwrite : Bool
}

///|
#cfg(target="native")
extern "C" fn exit(code : Int) = "exit"

///|
#cfg(target="wasm")
fn exit(code : UInt) = "wasi_snapshot_preview1" "proc_exit"

///|
async fn[T] die(message : String) -> T {
  @stdio.stderr.write("\{message}\n") catch {
    _ => ()
  }
  exit(1)
  panic()
}

///|
async fn write_stdout(message : String) -> Unit {
  @stdio.stdout.write(message) catch {
    err => die("error: failed to write stdout: \{err}")
  }
}

///|
async fn overwrite_if_changed(
  path : String,
  source : String,
  result : String,
) -> Bool {
  if source == result {
    return false
  }
  @fs.write_file(path, result, create_mode=CreateOrTruncate, sync=Full)
  true
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
async fn moonfmt(args : ArrayView[String]) -> Unit {
  let options = parse_moonfmt_args(args) catch { err => die("\{err}") }
  let source = @fs.read_file(options.path).text() catch { err => die("\{err}") }
  let result = @fmt.format(source)
  if options.overwrite {
    let second = @fmt.format(result)
    if second != result {
      die(
        "error: formatter output is not stable; left `\{options.path}` unchanged",
      )
    }
    ignore(overwrite_if_changed(options.path, source, result)) catch {
      err => die("error: failed to overwrite `\{options.path}`: \{err}")
    }
  } else {
    write_stdout(result)
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
async fn main {
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
