///|
fn ends_with(str : String, suffix : String) -> Bool {
  let str_len = str.length()
  let suffix_len = suffix.length()
  str_len >= suffix_len && str[str_len - suffix_len:].to_owned() == suffix
}

///|
fn config_kind(name : String) -> String? {
  if ends_with(name, "moon.pkg") {
    Some("pkg")
  } else if ends_with(name, "moon.mod") {
    Some("mod")
  } else if ends_with(name, "moon.work") {
    Some("work")
  } else {
    None
  }
}

///|
fn file_type_kind(file_type : String) -> String? {
  match file_type {
    "pkg" => Some("pkg")
    "mod" => Some("mod")
    "work" => Some("work")
    _ => None
  }
}

///|
fn kind_name(kind : String) -> String {
  match kind {
    "pkg" => "moon.pkg"
    "mod" => "moon.mod"
    "work" => "moon.work"
    _ => "moon.pkg"
  }
}

///|
fn parse_config(kind : String, source : String) -> @config.Ast {
  let name = kind_name(kind)
  let (ast, _) = match kind {
    "pkg" => @config.parse_moon_pkg(name~, source)
    "mod" => @config.parse_moon_mod(name~, source)
    "work" => @config.parse_moon_work(name~, source)
    _ => @config.parse_moon_pkg(name~, source)
  }
  ast
}

///|
enum InputSource {
  File(String)
  Stdin
  Code(String)
}

///|
struct LegacyOptions {
  source : InputSource
  file_type : String?
  output : String?
}

///|
enum CliAction {
  RunLegacy(LegacyOptions)
  Help(String)
  Error(String)
}

///|
fn root_usage() -> String {
  (
    #|Usage: mq <command>
    #|
    #|MoonBit config tool
    #|
    #|Commands:
    #|  legacy  Convert Moon config DSL to legacy-compatible JSON output
    #|  help    Print help for the subcommand.
    #|
    #|Options:
    #|  -h, --help  Show help information.
  )
}

///|
fn legacy_usage() -> String {
  (
    #|Usage: mq legacy [options] [input]
    #|
    #|Convert Moon config DSL to legacy-compatible JSON output
    #|
    #|Arguments:
    #|  input  Path to moon.pkg/moon.mod/moon.work, or - for stdin
    #|
    #|Options:
    #|  -h, --help               Show help information.
    #|  --file-type <file-type>  File type for stdin or -c input: pkg, mod, or work
    #|  -c, --code <source>      Read config source from command line
    #|  -o, --output <output>    Write output to file instead of stdout
  )
}

///|
fn cli_error(message : String, usage : String) -> CliAction {
  Error("error: \{message}\n\n\{usage}")
}

///|
fn is_help_arg(arg : String) -> Bool {
  arg == "-h" || arg == "--help"
}

///|
fn parse_cli(args : ArrayView[String]) -> CliAction {
  if args.length() == 0 {
    return cli_error("expected subcommand: legacy", root_usage())
  }
  let first = args[0]
  if is_help_arg(first) {
    return Help(root_usage())
  }
  if first == "help" {
    if args.length() == 1 {
      return Help(root_usage())
    } else if args.length() == 2 && args[1] == "legacy" {
      return Help(legacy_usage())
    } else {
      return cli_error("unknown help topic", root_usage())
    }
  }
  if first != "legacy" {
    return cli_error("unknown subcommand: \{first}", root_usage())
  }
  parse_legacy(args[1:])
}

///|
fn parse_legacy(args : ArrayView[String]) -> CliAction {
  let mut input : String? = None
  let mut code : String? = None
  let mut file_type : String? = None
  let mut output : String? = None
  let mut i = 0
  while i < args.length() {
    let arg = args[i]
    if is_help_arg(arg) {
      return Help(legacy_usage())
    } else if arg == "--file-type" {
      if i + 1 >= args.length() {
        return cli_error("missing value for --file-type", legacy_usage())
      }
      file_type = Some(args[i + 1])
      i = i + 2
      continue
    } else if arg.has_prefix("--file-type=") {
      file_type = Some(arg["--file-type=".length():].to_owned())
      i = i + 1
      continue
    } else if arg == "-c" || arg == "--code" {
      if i + 1 >= args.length() {
        return cli_error("missing value for \{arg}", legacy_usage())
      }
      code = Some(args[i + 1])
      i = i + 2
      continue
    } else if arg.has_prefix("--code=") {
      code = Some(arg["--code=".length():].to_owned())
      i = i + 1
      continue
    } else if arg == "-o" || arg == "--output" {
      if i + 1 >= args.length() {
        return cli_error("missing value for \{arg}", legacy_usage())
      }
      output = Some(args[i + 1])
      i = i + 2
      continue
    } else if arg.has_prefix("--output=") {
      output = Some(arg["--output=".length():].to_owned())
      i = i + 1
      continue
    } else if arg.has_prefix("-") && arg != "-" {
      return cli_error("unknown option: \{arg}", legacy_usage())
    } else {
      if input is Some(_) {
        return cli_error("unexpected argument: \{arg}", legacy_usage())
      }
      input = Some(arg)
      i = i + 1
    }
  }
  if code is Some(source) {
    if input is Some(_) {
      return cli_error(
        "-c/--code cannot be used with an input path",
        legacy_usage(),
      )
    }
    return RunLegacy({ source: Code(source), file_type, output })
  }
  let source = match input {
    Some("-") => Stdin
    Some(path) => File(path)
    None => return cli_error("missing input", legacy_usage())
  }
  RunLegacy({ source, file_type, output })
}

///|
fn kind_from_options(options : LegacyOptions) -> String? {
  match options.file_type {
    Some(file_type) =>
      match file_type_kind(file_type) {
        Some(kind) => Some(kind)
        None => None
      }
    None =>
      match options.source {
        File(path) => config_kind(path)
        Stdin | Code(_) => None
      }
  }
}
