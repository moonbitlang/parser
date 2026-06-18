///|
fn post_process_moon_pkg(ast : Ast) -> Ast {
  match ast {
    Obj(fields, loc~) => {
      let results : Array[(String, Ast)] = []
      for field in fields {
        let (key, value) = field
        match key {
          "import" | "wbtest-import" | "test-import" | "dev_build" | "rule" =>
            results.push(field)
          "warnings" =>
            match value {
              Str(_, ..) => results.push(("warn-list", value))
              _ => results.push(field)
            }
          "supported_targets" =>
            match value {
              Str(_, ..) => results.push(("supported-targets", value))
              _ => results.push(field)
            }
          "options" =>
            match value {
              Obj(option_fields, ..) =>
                for option_field in option_fields {
                  results.push(option_field)
                }
              _ => results.push(field)
            }
          _ => results.push(field)
        }
      }
      Obj(Vector(results), loc~)
    }
    _ => ast
  }
}

///|
fn post_process_moon_mod(ast : Ast) -> Ast {
  match ast {
    Obj(fields, loc~) => {
      let split_import_spec = fn(spec : String) -> (String, String) {
        match spec.rev_find("@") {
          Some(index) if index > 0 =>
            (
              spec.unsafe_substring(start=0, end=index),
              spec.unsafe_substring(start=index + 1, end=spec.length()),
            )
          _ => (spec, "")
        }
      }
      let results : Array[(String, Ast)] = []
      for field in fields {
        let (key, value) = field
        match key {
          "name"
          | "version"
          | "readme"
          | "repository"
          | "license"
          | "keywords"
          | "description"
          | "rule" => results.push(field)
          "preferred_target" => results.push(("preferred-target", value))
          "warnings" =>
            match value {
              Str(_, ..) => results.push(("warn-list", value))
              _ => results.push(field)
            }
          "supported_targets" =>
            match value {
              Str(_, ..) => results.push(("supported-targets", value))
              Arr(_, ..) => results.push(("supported-targets", value))
              _ => results.push(field)
            }
          "import" =>
            match value {
              Arr(imports, loc=import_loc) => {
                let deps : Array[(String, Ast)] = []
                for item in imports {
                  match item {
                    Str(spec, loc~) => {
                      let (name, version) = split_import_spec(spec)
                      deps.push((name, Str(version, loc~)))
                    }
                    _ => ()
                  }
                }
                results.push(("deps", Obj(Vector(deps), loc=import_loc)))
              }
              _ => results.push(field)
            }
          "options" =>
            match value {
              Obj(option_fields, ..) =>
                for option_field in option_fields {
                  results.push(option_field)
                }
              _ => results.push(field)
            }
          _ => results.push(field)
        }
      }
      Obj(Vector(results), loc~)
    }
    _ => ast
  }
}

///|
fn post_process_moon_work(ast : Ast) -> Ast {
  ast
}
