///|
fn die(message : String) -> Unit {
  write_stderr("\{message}\n")
  @miniio.exit(1)
}

///|
fn write_stderr(message : String) -> Unit {
  @miniio.stderr.write_text(message) catch {
    _ => ()
  }
}

///|
fn write_stdout(message : String) -> Unit {
  try @miniio.stdout.write_text(message) catch {
    err => die("error: failed to write stdout: \{err}")
  } noraise {
    _ => ()
  }
}

///|
fn program_args() -> ArrayView[String] {
  try @miniio.args_get() catch {
    err => {
      die("error: failed to read argv: \{err}")
      []
    }
  } noraise {
    args => if args.length() > 1 { args[1:] } else { [] }
  }
}

///|
fn read_file(path : String) -> String {
  try @miniio.read_text_file(path) catch {
    err => {
      die("error: failed to read \{path}: \{err}")
      ""
    }
  } noraise {
    source => source
  }
}

///|
fn read_stdin() -> String {
  try @miniio.stdin.read_all().text() catch {
    err => {
      die("error: failed to read stdin: \{err}")
      ""
    }
  } noraise {
    source => source
  }
}

///|
fn write_result(output : String?, content : String) -> Unit {
  let content = "\{content}\n"
  match output {
    Some(path) =>
      try
        @miniio.write_text_file(path, content, create_mode=CreateOrTruncate)
      catch {
        err => die("error: failed to write \{path}: \{err}")
      } noraise {
        _ => ()
      }
    None => write_stdout(content)
  }
}

///|
fn run_legacy(options : LegacyOptions) -> Unit {
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
fn main {
  match parse_cli(program_args()) {
    Help(text) => {
      write_stdout("\{text}\n")
      @miniio.exit(0)
    }
    Error(message) => die(message)
    RunLegacy(options) => run_legacy(options)
  }
}
