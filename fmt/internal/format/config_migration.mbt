///|
fn config_apply(
  name : String,
  args : Array[@moon_config.Field],
  name_loc~ : Location = config_dummy_location(),
  args_loc~ : Location = config_dummy_location(),
) -> @moon_config.Statement {
  Expr(
    Apply(name~, args~, name_loc~, args_loc~, loc={
      start: name_loc.start,
      end: args_loc.end,
    }),
  )
}

///|
fn direct_pkg_option_key(key : String) -> String? {
  match key {
    "proof-enabled" | "proof_enabled" => Some("proof_enabled")
    "max-concurrent-tests" | "max_concurrent_tests" =>
      Some("max_concurrent_tests")
    "regex-backend" | "regex_backend" => Some("regex_backend")
    _ => None
  }
}

///|
fn direct_mod_option_key(key : String) -> String? {
  match key {
    "name"
    | "version"
    | "private"
    | "readme"
    | "repository"
    | "license"
    | "keywords"
    | "description"
    | "source" => Some(key)
    "warn-list" => Some("warnings")
    "supported-targets" => Some("supported_targets")
    "preferred-target" => Some("preferred_target")
    _ => None
  }
}

///|
fn config_virtual_args(
  fields : Array[@moon_config.Field],
) -> Array[@moon_config.Field] {
  fields.map(field => {
    if field.key == "has-default" {
      { ..field, key: "has_default", }
    } else {
      field
    }
  })
}

///|
fn config_is_virtual_decl(field : @moon_config.Field) -> Bool {
  (field.key == "virtual" || field.key == "virtual_pkg") &&
  field.value is Map(_)
}

///|
fn migrate_config(
  ast : @moon_config.Ast,
  kind : ConfigKind,
) -> @moon_config.Ast {
  let has_virtual = ast.0
    .iter()
    .any(statement => {
      match statement {
        Expr(Apply(name="virtual", ..)) => true
        Expr(Apply(name="options", args~, ..)) =>
          args.any(config_is_virtual_decl)
        _ => false
      }
    })
  let statements : Array[@moon_config.Statement] = []
  for statement in ast.0 {
    match statement {
      Expr(Apply(name="options", args~, name_loc~, args_loc~, loc~)) => {
        let remaining = []
        for field in args {
          let value_loc = field.value.loc()
          match (kind, field.key, field.value) {
            (MoonPkg, "is-main" | "is_main" | "link", Bool(value=true, ..)) => {
              let pkgtype = if field.key == "link" {
                "foreign_library"
              } else {
                "executable"
              }
              statements.push(
                config_apply(
                  "pkgtype",
                  [
                    {
                      key: "kind",
                      key_loc: value_loc,
                      value: String(str=pkgtype, loc=value_loc),
                    },
                  ],
                  name_loc=field.key_loc,
                  args_loc=value_loc,
                ),
              )
            }
            (MoonPkg, "warn-list", _) =>
              statements.push(Assign({ ..field, key: "warnings", }))
            (MoonPkg, "supported-targets", _) =>
              statements.push(Assign({ ..field, key: "supported_targets", }))
            (MoonPkg, "formatter", Map(fields~, loc~)) =>
              statements.push(
                config_apply(
                  "formatter",
                  fields,
                  name_loc=field.key_loc,
                  args_loc=loc,
                ),
              )
            (MoonPkg, "implement", String(_)) if !has_virtual =>
              statements.push(
                config_apply(
                  "virtual",
                  [field],
                  name_loc=field.key_loc,
                  args_loc=value_loc,
                ),
              )
            (MoonPkg, "virtual" | "virtual_pkg", Map(fields~, loc~)) =>
              statements.push(
                config_apply(
                  "virtual",
                  config_virtual_args(fields),
                  name_loc=field.key_loc,
                  args_loc=loc,
                ),
              )
            _ => {
              let key = match kind {
                MoonPkg => direct_pkg_option_key(field.key)
                MoonMod => direct_mod_option_key(field.key)
              }
              match key {
                Some(key) => statements.push(Assign({ ..field, key, }))
                None => remaining.push(field)
              }
            }
          }
        }
        if !remaining.is_empty() {
          statements.push(
            Expr(
              Apply(name="options", args=remaining, name_loc~, args_loc~, loc~),
            ),
          )
        }
      }
      _ => statements.push(statement)
    }
  }
  @moon_config.Ast(@vector.Vector(statements))
}

///|
fn config_escape_json_string(value : String) -> String {
  let quoted = Json::string(value).stringify()
  quoted[1:quoted.length() - 1].to_owned()
}

///|
fn json_to_config_expr(value : Json) -> @moon_config.Expr {
  let loc = config_dummy_location()
  match value {
    Null => String(str="", loc~)
    True => Bool(value=true, loc~)
    False => Bool(value=false, loc~)
    String(value) => String(str=config_escape_json_string(value), loc~)
    Number(number, repr~) => Int(repr=repr.unwrap_or(number.to_string()), loc~)
    Array(items) => Array(items=items.map(json_to_config_expr), loc~)
    Object(fields) =>
      Map(
        fields=fields
          .iter()
          .map(pair => json_to_config_field(pair.0, pair.1))
          .to_array(),
        loc~,
      )
  }
}

///|
fn json_to_config_field(key : String, value : Json) -> @moon_config.Field {
  {
    key: config_escape_json_string(key),
    key_loc: config_dummy_location(),
    value: json_to_config_expr(value),
  }
}

///|
fn config_import_item(
  path : String,
  alias_ : String?,
) -> @moon_config.ImportItem {
  let loc = config_dummy_location()
  {
    path: config_escape_json_string(path),
    path_loc: loc,
    alias_: alias_.map(name => (name, loc)),
    loc,
  }
}

///|
fn config_import(
  kind : @moon_config.ImportKind,
  items : Array[@moon_config.ImportItem],
) -> @moon_config.Statement {
  let loc = config_dummy_location()
  Import(kind~, items~, conditions=[], keyword_loc=loc, list_loc=loc, loc~)
}

///|
fn migrate_json_imports(value : Json) -> Array[@moon_config.ImportItem] {
  match value {
    Array(items) =>
      items.filter_map(item => {
        match item {
          String(path) => Some(config_import_item(path, None))
          Object(fields) =>
            match fields.get("path") {
              Some(String(path)) => {
                let alias_ = match fields.get("alias") {
                  Some(String(name)) => Some(name)
                  _ => None
                }
                Some(config_import_item(path, alias_))
              }
              _ => None
            }
          _ => None
        }
      })
    _ => []
  }
}

///|
priv suberror ConfigMigrationError {
  InvalidConfig(String)
}

///|
fn migrate_json_config(
  json : Json,
  kind : ConfigKind,
) -> @moon_config.Ast raise ConfigMigrationError {
  guard json is Object(fields) else {
    raise InvalidConfig("Expected a configuration JSON object")
  }
  let statements : Array[@moon_config.Statement] = []
  let options = []
  let has_virtual = fields
    .iter()
    .any(pair => config_is_virtual_decl(json_to_config_field(pair.0, pair.1)))
  for key, value in fields {
    let field = json_to_config_field(key, value)
    match (kind, key) {
      (_, "warn-list") => statements.push(Assign({ ..field, key: "warnings", }))
      (_, "supported-targets") =>
        statements.push(Assign({ ..field, key: "supported_targets", }))
      (MoonPkg, "import" | "test-import" | "wbtest-import") => {
        let import_kind : @moon_config.ImportKind = match key {
          "test-import" => Test
          "wbtest-import" => Wbtest
          _ => Normal
        }
        statements.push(config_import(import_kind, migrate_json_imports(value)))
      }
      (MoonPkg, "implement") if !has_virtual && field.value is String(_) =>
        statements.push(config_apply("virtual", [field]))
      (MoonPkg, "virtual" | "virtual_pkg") =>
        match field.value {
          Map(fields~, ..) =>
            statements.push(
              config_apply("virtual", config_virtual_args(fields)),
            )
          _ => options.push(field)
        }
      (MoonPkg, "rule" | "pkgtype" | "dev_build" | "formatter")
      | (MoonMod, "rule") =>
        match field.value {
          Map(fields~, ..) => statements.push(config_apply(key, fields))
          _ => raise InvalidConfig("Expected an object for `\{key}`")
        }
      (MoonMod, "deps") =>
        if value is Object(deps) && !deps.is_empty() {
          let items = deps
            .iter()
            .map(pair => {
              let path = match pair.1 {
                String(version) if version != "" => pair.0 + "@" + version
                _ => pair.0
              }
              config_import_item(path, None)
            })
            .to_array()
          statements.push(config_import(Normal, items))
        }
      _ => {
        let direct = match kind {
          MoonMod => direct_mod_option_key(key)
          MoonPkg => direct_pkg_option_key(key)
        }
        match direct {
          Some(key) => statements.push(Assign({ ..field, key, }))
          None => options.push(field)
        }
      }
    }
  }
  if !options.is_empty() {
    statements.push(config_apply("options", options))
  }
  @moon_config.Ast(@vector.Vector(statements))
}
