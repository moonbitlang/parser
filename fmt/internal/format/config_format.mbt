///|
fn json_string_literal(value : String) -> String {
  Json::string(value).stringify()
}

///|
fn config_label(label : String) -> String {
  if label.contains("-") || label.contains("\"") || label.contains("\n") {
    json_string_literal(label)
  } else {
    label
  }
}

///|
fn sorted_json_fields(fields : Map[String, Json]) -> Array[(String, Json)] {
  let items = fields.to_array()
  items.sort_by(fn(a, b) { a.0.compare(b.0) })
  items
}

///|
fn json_inline(value : Json) -> String {
  match value {
    Null => "\"\""
    True => "true"
    False => "false"
    String(value) => json_string_literal(value)
    Number(number, repr~) => repr.unwrap_or(number.to_string())
    Array(items) =>
      if items.length() == 0 {
        "[]"
      } else {
        "[ " + items.map(json_inline)[:].join(", ") + " ]"
      }
    Object(fields) => {
      let items = sorted_json_fields(fields)
      if items.length() == 0 {
        "{}"
      } else {
        "{ " +
        items.map(fn(item) {
          let (key, value) = item
          "\{config_label(key)}: \{json_inline(value)}"
        })[:].join(", ") +
        " }"
      }
    }
  }
}

///|
fn json_top_value(value : Json) -> String {
  match value {
    Array(items) =>
      if items.length() == 0 {
        "[]"
      } else {
        "[\n" +
        items.map(item => "  \{json_inline(item)},")[:].join("\n") +
        "\n]"
      }
    _ => json_inline(value)
  }
}

///|
fn object_get(fields : Map[String, Json], key : String) -> Json? {
  fields.get(key)
}

///|
fn first_field(fields : Map[String, Json], keys : Array[String]) -> Json? {
  for key in keys {
    match object_get(fields, key) {
      Some(value) => return Some(value)
      None => ()
    }
  }
  None
}

///|
fn render_assignment(name : String, value : Json) -> String {
  "\{name} = \{json_top_value(value)}"
}

///|
fn split_dep_spec(spec : String) -> (String, String) {
  match spec.rev_find("@") {
    Some(index) if index > 0 =>
      (
        spec.unsafe_substring(start=0, end=index),
        spec.unsafe_substring(start=index + 1, end=spec.length()),
      )
    _ => (spec, "")
  }
}

///|
fn render_dep_spec(name : String, value : Json) -> String {
  match value {
    String(version) if version != "" => "\{name}@\{version}"
    _ => name
  }
}

///|
fn render_import_block(deps : Json) -> String? {
  match deps {
    Object(fields) => {
      let items = sorted_json_fields(fields)
      if items.length() == 0 {
        None
      } else {
        let specs = items.map(fn(item) {
          let (name, value) = item
          "  \{json_string_literal(render_dep_spec(name, value))},"
        })
        Some("import {\n\{specs[:].join("\n")}\n}")
      }
    }
    _ => None
  }
}

///|
fn render_options(fields : Map[String, Json], known : Array[String]) -> String? {
  let options : Array[(String, Json)] = []
  for item in sorted_json_fields(fields) {
    let (key, value) = item
    if !known.contains(key) {
      options.push((key, value))
    }
  }
  if options.length() == 0 {
    None
  } else {
    let lines = options.map(fn(item) {
      let (key, value) = item
      "  \{config_label(key)}: \{json_inline(value)},"
    })
    Some("options(\n\{lines[:].join("\n")}\n)")
  }
}

///|
fn render_moon_pkg_json(json : Json) -> String {
  match json {
    Object(fields) => {
      let sections : Array[String] = []
      match first_field(fields, ["warn-list", "warnings"]) {
        Some(value) => sections.push(render_assignment("warnings", value))
        None => ()
      }
      match first_field(fields, ["supported-targets", "supported_targets"]) {
        Some(value) =>
          sections.push(render_assignment("supported_targets", value))
        None => ()
      }
      let known = [
        "import", "test-import", "wbtest-import", "warn-list", "warnings", "supported-targets",
        "supported_targets", "dev_build", "formatter", "rule",
      ]
      match render_options(fields, known) {
        Some(options) => sections.push(options)
        None => ()
      }
      sections[:].join("\n\n")
    }
    _ => ""
  }
}

///|
fn render_moon_mod_json(json : Json) -> String {
  match json {
    Object(fields) => {
      let sections : Array[String] = []
      match object_get(fields, "name") {
        Some(value) => sections.push(render_assignment("name", value))
        None => ()
      }
      match object_get(fields, "version") {
        Some(value) => sections.push(render_assignment("version", value))
        None => ()
      }
      if first_field(fields, ["deps", "import"]) is Some(value) {
        match render_import_block(value) {
          Some(imports) => sections.push(imports)
          None => ()
        }
      }
      if first_field(fields, ["warn-list", "warnings"]) is Some(value) {
        sections.push(render_assignment("warnings", value))
      }
      for
        key in [
          "readme", "repository", "license", "keywords", "description", "source",
        ] {
        if object_get(fields, key) is Some(value) {
          sections.push(render_assignment(key, value))
        }
      }
      if first_field(fields, ["supported-targets", "supported_targets"])
        is Some(value) {
        sections.push(render_assignment("supported_targets", value))
      }
      if first_field(fields, ["preferred-target", "preferred_target"])
        is Some(value) {
        sections.push(render_assignment("preferred_target", value))
      }
      let known = [
        "name", "version", "deps", "import", "warn-list", "warnings", "readme", "repository",
        "license", "keywords", "description", "source", "supported-targets", "supported_targets",
        "preferred-target", "preferred_target", "rule",
      ]
      if render_options(fields, known) is Some(options) {
        sections.push(options)
      }
      sections[:].join("\n\n")
    }
    _ => ""
  }
}

///|
fn apply_dependency_edits(
  json : Json,
  add_dependencies : ArrayView[String],
  remove_dependencies : ArrayView[String],
) -> Json {
  match json {
    Object(fields) => {
      let result : Map[String, Json] = Map([])
      for item in fields.to_array() {
        let (key, value) = item
        if key != "deps" {
          result[key] = value
        }
      }
      let deps : Map[String, Json] = match object_get(fields, "deps") {
        Some(Object(existing)) => {
          let copy : Map[String, Json] = Map([])
          for item in existing.to_array() {
            let (key, value) = item
            copy[key] = value
          }
          copy
        }
        _ => Map([])
      }
      for spec in remove_dependencies {
        let (name, _) = split_dep_spec(spec)
        deps.remove(name)
      }
      for spec in add_dependencies {
        let (name, version) = split_dep_spec(spec)
        deps[name] = Json::string(version)
      }
      if !deps.is_empty() {
        result["deps"] = Json::object(deps)
      }
      Json::object(result)
    }
    _ => json
  }
}

///|
pub fn format_moon_pkg(source : String) -> String {
  let (ast, reports) = @moon_config.parse_moon_pkg(source)
  if reports.length() != 0 {
    source
  } else {
    render_moon_pkg_json(ast.to_json())
  }
}

///|
pub fn format_moon_mod(
  source : String,
  add_dependencies? : ArrayView[String] = [],
  remove_dependencies? : ArrayView[String] = [],
) -> String {
  let (ast, reports) = @moon_config.parse_moon_mod(source)
  if reports.length() != 0 {
    source
  } else {
    ast.to_json()
    |> apply_dependency_edits(add_dependencies, remove_dependencies)
    |> render_moon_mod_json
  }
}

///|
pub fn format_json_to_moon_pkg(source : String) -> String raise {
  @json.parse(source) |> render_moon_pkg_json
}

///|
pub fn format_json_to_moon_mod(source : String) -> String raise {
  @json.parse(source) |> render_moon_mod_json
}
