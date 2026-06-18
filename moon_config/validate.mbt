///|
fn emit_invalid_config(
  diagnostics : Array[Report],
  config_name : String,
  key : String,
  value : Ast,
) -> Unit {
  diagnostics.push({
    loc: value.loc(),
    msg: "Invalid \{config_name} config: unexpected key `\{key}`.",
  })
}

///|
fn check_unique_toplevel(
  seen : Map[String, Bool],
  diagnostics : Array[Report],
  config_name : String,
  allowed_duplicate : (String) -> Bool,
  key : String,
  value : Ast,
) -> Unit {
  if seen.contains(key) && !allowed_duplicate(key) {
    diagnostics.push({
      loc: value.loc(),
      msg: "Duplicate key `\{key}` found in \{config_name}.",
    })
  }
  seen[key] = true
}

///|
fn _validate_moon_pkg(ast : Ast, diagnostics : Array[Report]) -> Unit {
  match ast {
    Obj(fields, ..) => {
      let allowed_duplicate = fn(key : String) -> Bool {
        key == "dev_build" || key == "rule"
      }
      let seen : Map[String, Bool] = Map([])
      for field in fields {
        let (key, value) = field
        check_unique_toplevel(
          seen, diagnostics, "moon.pkg", allowed_duplicate, key, value,
        )
        match key {
          "warnings" | "supported_targets" | "options" =>
            emit_invalid_config(diagnostics, "moon.pkg", key, value)
          _ => ()
        }
      }
    }
    _ => ()
  }
}

///|
fn _validate_moon_mod(ast : Ast, diagnostics : Array[Report]) -> Unit {
  match ast {
    Obj(fields, ..) => {
      let seen : Map[String, Bool] = Map([])
      let allowed_duplicate = fn(key : String) -> Bool { key == "rule" }
      for field in fields {
        let (key, value) = field
        check_unique_toplevel(
          seen, diagnostics, "moon.mod", allowed_duplicate, key, value,
        )
        match key {
          "warnings"
          | "supported_targets"
          | "preferred_target"
          | "import"
          | "options" =>
            emit_invalid_config(diagnostics, "moon.mod", key, value)
          _ => ()
        }
      }
    }
    _ => ()
  }
}

///|
fn _validate_moon_work(ast : Ast, diagnostics : Array[Report]) -> Unit {
  match ast {
    Obj(fields, ..) => {
      let seen : Map[String, Bool] = Map([])
      let allowed_duplicate = fn(_ : String) -> Bool { false }
      for field in fields {
        let (key, value) = field
        check_unique_toplevel(
          seen, diagnostics, "moon.work", allowed_duplicate, key, value,
        )
        match key {
          "members" | "preferred_target" => ()
          _ => emit_invalid_config(diagnostics, "moon.work", key, value)
        }
      }
    }
    _ => ()
  }
}
