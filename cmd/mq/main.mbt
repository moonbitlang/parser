///|
async fn die(message : String) -> Unit {
  @stdio.stderr.write("\{message}\n") catch {
    _ => ()
  }
  @sys.exit(1)
}

///|
async fn write_stdout(message : String) -> Unit {
  try @stdio.stdout.write(message) catch {
    err => die("error: failed to write stdout: \{err}")
  } noraise {
    _ => ()
  }
}

///|
fn program_args() -> ArrayView[String] {
  let args = @sys.get_cli_args()
  if args.length() > 1 {
    args[1:]
  } else {
    []
  }
}

///|
async fn read_file(path : String) -> String {
  try @fs.read_file(path).text() catch {
    err => {
      die("error: failed to read \{path}: \{err}")
      ""
    }
  } noraise {
    source => source
  }
}

///|
async fn read_stdin() -> String {
  try @stdio.stdin.read_all().text() catch {
    err => {
      die("error: failed to read stdin: \{err}")
      ""
    }
  } noraise {
    source => source
  }
}

///|
async fn write_result(output : String?, content : String) -> Unit {
  let content = "\{content}\n"
  match output {
    Some(path) =>
      try @fs.write_file(path, content, create_mode=CreateOrTruncate) catch {
        err => die("error: failed to write \{path}: \{err}")
      } noraise {
        _ => ()
      }
    None => write_stdout(content)
  }
}

///|
async fn run_legacy(options : LegacyOptions) -> Unit {
  let kind = match kind_from_options(options) {
    Some(kind) => kind
    None => {
      die("error: --file-type pkg|mod|work is required for stdin or -c input")
      "pkg"
    }
  }
  let source = match options.source {
    File(path) => read_file(path)
    Stdin => read_stdin()
    Code(source) => source
  }
  let ast = parse_config(kind, source)
  write_result(options.output, ast.to_json().stringify())
}

///|
async fn main {
  match parse_cli(program_args()) {
    Help(text) => {
      write_stdout("\{text}\n")
      @sys.exit(0)
    }
    Error(message) => die(message)
    RunLegacy(options) => run_legacy(options)
  }
}
