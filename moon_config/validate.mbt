///|
priv enum ConfigKind {
  Module
  Package
  Workspace
}

///|
fn config_name(kind : ConfigKind) -> String {
  match kind {
    Module => "moon.mod"
    Package => "moon.pkg"
    Workspace => "moon.work"
  }
}

///|
fn validate_config(
  ast : Ast,
  kind : ConfigKind,
  diagnostics : Diagnostics,
) -> Unit {
  let seen : Map[String, Bool] = Map([])
  let filename = config_name(kind)
  fn invalid(key : String, loc : Location) -> Unit {
    diagnostics.errors.push({
      loc,
      msg: "Invalid configuration `\{key}` in \{filename} file.",
    })
  }
  fn unique(key : String, loc : Location) -> Unit {
    let repeatable = (key == "rule" && !(kind is Workspace)) ||
      (key == "dev_build" && kind is Package)
    if seen.contains(key) && !repeatable {
      invalid(key, loc)
    }
    seen[key] = true
  }
  fn validate_value(key : String, value : Expr, loc : Location) -> Unit {
    unique(key, loc)
    match (kind, key, value) {
      (
        Module,
        "name"
        | "version"
        | "rule"
        | "readme"
        | "repository"
        | "license"
        | "keywords"
        | "description"
        | "source",
        _,
      ) => ()
      (Module, "private", Bool(_)) => ()
      (Module, "preferred_target" | "warnings" | "supported_targets", String(_)) =>
        ()
      (
        Package,
        "import"
        | "test-import"
        | "wbtest-import"
        | "dev_build"
        | "rule"
        | "formatter",
        _,
      ) => ()
      (Package, "warnings" | "supported_targets" | "regex_backend", String(_)) =>
        ()
      (Package, "proof_enabled", Bool(_)) => ()
      (Package, "max_concurrent_tests", Int(_)) => ()
      (Package, "pkgtype", value) => {
        let valid = match value {
          Map(fields~, ..) | Apply(args=fields, ..) =>
            find_field(fields, "kind")
            is Some(
              String(str="library" | "executable" | "foreign_library", ..)
            )
          _ => false
        }
        if !valid {
          diagnostics.errors.push({
            loc,
            msg: "Invalid `pkgtype` in moon.pkg file. Expected `pkgtype(kind: \"library\" | \"executable\" | \"foreign_library\")`.",
          })
        }
      }
      (Package, "virtual", value) => {
        let valid = match value {
          Map(fields~, ..) | Apply(args=fields, ..) =>
            if find_field(fields, "implement") is Some(implementation) {
              unique("implement", loc)
              fields.length() == 1 && implementation is String(_)
            } else if find_field(fields, "has_default") is Some(flag) {
              flag is Bool(_)
            } else {
              find_field(fields, "has-default") is Some(Bool(_))
            }
          _ => false
        }
        if !valid {
          invalid(key, loc)
        }
      }
      (Module | Package, "options", Apply(args~, ..)) =>
        for field in args {
          let field_loc = field.value.loc()
          let field_key = if kind is Package {
            match (field.key, field.value) {
              ("is-main" | "is_main" | "link", Bool(value=true, ..)) =>
                "pkgtype"
              ("proof-enabled", _) => "proof_enabled"
              ("max-concurrent-tests", _) => "max_concurrent_tests"
              ("regex-backend", _) => "regex_backend"
              ("virtual_pkg", _) => "virtual"
              _ => field.key
            }
          } else {
            field.key
          }
          unique(field_key, field_loc)
          if kind is Module &&
            field.key == "private" &&
            !(field.value is Bool(_)) {
            invalid(field.key, field_loc)
          }
          diagnose_option(kind, field, diagnostics.warnings)
        }
      (Workspace, "members" | "preferred_target", _) => ()
      _ => invalid(key, loc)
    }
  }
  for statement in ast.0 {
    match statement {
      Import(kind=import_kind, loc~, ..) => {
        let key = import_kind_to_key(import_kind)
        unique(key, loc)
        if kind is Workspace || (kind is Module && !(import_kind is Normal)) {
          invalid(key, loc)
        }
      }
      Assign(field) => validate_value(field.key, field.value, field.value.loc())
      Expr(Apply(name~, args_loc~, ..) as value) =>
        validate_value(name, value, args_loc)
      Expr(value) => validate_value("", value, value.loc())
    }
  }
}

///|
fn diagnose_option(
  kind : ConfigKind,
  field : Field,
  warnings : Array[Report],
) -> Unit {
  let replacement = match (kind, field.key) {
    (Module, "private") => Some(("specify module privacy", "private = true"))
    (Module, "readme") => Some(("specify the README path", "readme = \"...\""))
    (Module, "repository") =>
      Some(("specify the repository URL", "repository = \"...\""))
    (Module, "license") => Some(("specify the license", "license = \"...\""))
    (Module, "keywords") => Some(("specify keywords", "keywords = [...]"))
    (Module, "description") =>
      Some(("specify the description", "description = \"...\""))
    (Module, "source") =>
      Some(("specify the source directory", "source = \"...\""))
    (Module, "preferred-target") =>
      Some(("specify the preferred target", "preferred_target = \"...\""))
    (Module | Package, "supported-targets") =>
      Some(("specify supported targets", "supported_targets = \"...\""))
    (Module | Package, "warn-list") =>
      Some(("specify warning options", "warnings = \"...\""))
    (Module | Package, "import") =>
      Some(("import packages", "import { \"path/to/pkg1\", \"path/to/pkg2\" }"))
    (Package, "formatter") =>
      Some(("configure formatter options", "formatter(ignore: [\"file\"])"))
    (Package, "test-import") =>
      Some(
        (
          "import packages for black-box test", "import { \"path/to/pkg\" } for \"test\"",
        ),
      )
    (Package, "wbtest-import") =>
      Some(
        (
          "import packages for white-box test", "import { \"path/to/pkg\" } for \"wbtest\"",
        ),
      )
    _ => None
  }
  if replacement is Some((purpose, new_config)) {
    warnings.push({
      loc: field.value.loc(),
      msg: "The syntax `\"\{field.key}\": ...` for \{purpose} is deprecated. Use `\{new_config}` in top-level instead.",
    })
  }
}
