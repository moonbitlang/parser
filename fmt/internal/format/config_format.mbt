///|
priv enum ConfigKind {
  MoonPkg
  MoonMod
}

///|
priv struct ConfigParseResult {
  ast : @moon_config.Ast
  tokens : @tokens.Triples
  ordinary_comments : Array[String]
}

///|
priv struct ConfigFmt {
  mapper : @comment.CheckpointMapper?
  migration : Bool
}

///|
fn ConfigFmt::new(
  mapper? : @comment.CheckpointMapper,
  migration? : Bool = false,
) -> Self {
  { mapper, migration }
}

///|
fn ConfigFmt::checkpoint(self : Self, pos : Position) -> Doc {
  if self.mapper is Some(mapper) {
    mapper.checkpoint(pos)
  }
  empty
}

///|
fn ConfigFmt::token(self : Self, kind : TokenKind) -> Doc {
  if self.mapper is Some(mapper) {
    mapper.token(kind)
  }
  empty
}

///|
fn ConfigFmt::placeholder(self : Self) -> Doc {
  match self.mapper {
    Some(mapper) => mapper.placeholder()
    None => empty
  }
}

///|
fn ConfigFmt::eof(self : Self) -> Doc {
  if self.mapper is Some(mapper) {
    mapper.eof()
  }
  empty
}

///|
fn config_dummy_position() -> Position {
  { fname: "", lnum: -1, bol: 0, cnum: 0 }
}

///|
fn config_dummy_location() -> Location {
  let pos = config_dummy_position()
  { start: pos, end: pos }
}

///|
fn config_is_virtual(ast : @moon_config.Ast) -> Bool {
  ast.loc().start.lnum < 0
}

///|
fn config_string_literal(value : String, migration : Bool) -> String {
  if migration {
    Json::string(value).stringify()
  } else {
    "\"" + value + "\""
  }
}

///|
fn config_label_is_plain(label : String) -> Bool {
  let { tokens, errors, .. } = @lexer.tokens_from_string(comment=false, label)
  if !errors.is_empty() {
    return false
  }
  let mut found = false
  for triple in tokens {
    match triple.0 {
      LIDENT(name) if !found && name == label => found = true
      EOF | NEWLINE | SEMI(_) => ()
      _ => return false
    }
  }
  found
}

///|
fn config_label(label : String, migration : Bool) -> Doc {
  if config_label_is_plain(label) {
    text(label)
  } else {
    text(config_string_literal(label, migration))
  }
}

///|
fn config_map_key(label : String, migration : Bool) -> Doc {
  text(config_string_literal(label, migration))
}

///|
fn ConfigFmt::consume_label(self : Self) -> Unit {
  // Both spellings are accepted for labels. Trying the other spelling after a
  // successful match is harmless because the cursor is then at `:`.
  ignore(self.token(TK_LIDENT))
  ignore(self.token(TK_STRING))
}

///|
fn ConfigFmt::value(self : Self, ast : @moon_config.Ast) -> Doc {
  match ast {
    Null(_) => text("\"\"")
    Bool(value, ..) => text(if value { "true" } else { "false" })
    Str(value, ..) => text(config_string_literal(value, self.migration))
    Float(value, ..) => text(value)
    Arr(items, ..) => self.array(items)
    Obj(fields, ..) => self.map(fields)
  }
}

///|
fn ConfigFmt::array(self : Self, items : Vector[@moon_config.Ast]) -> Doc {
  ignore(self.token(TK_LBRACKET))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, item in items.to_array() {
    if !config_is_virtual(item) {
      ignore(self.checkpoint(item.loc().start))
    }
    let item_doc = self.value(item)
    let after = self.placeholder()
    if !config_is_virtual(item) {
      ignore(self.checkpoint(item.loc().end))
    }
    ignore(self.token(TK_COMMA))
    let separator_comments = self.placeholder()
    let printed_comma = if index + 1 < items.length() {
      comma
    } else {
      switch(empty, comma)
    }
    let following_line = if index + 1 < items.length() { line } else { empty }
    docs.push(
      item_doc + after + printed_comma + separator_comments + following_line,
    )
  }
  ignore(self.token(TK_RBRACKET))
  let body = if docs.is_empty() {
    opening_comments
  } else {
    opening_comments + docs.fold(init=empty, Doc::add)
  }
  if items.is_empty() {
    char('[') + body + char(']')
  } else {
    group(char('[') + nest(line + body) + line + char(']'))
  }
}

///|
fn ConfigFmt::map_field(
  self : Self,
  key : String,
  value : @moon_config.Ast,
) -> Doc {
  ignore(self.token(TK_STRING))
  ignore(self.token(TK_COLON))
  let before_value = self.placeholder()
  if !config_is_virtual(value) {
    ignore(self.checkpoint(value.loc().start))
  }
  config_map_key(key, self.migration) +
  colon +
  space +
  before_value +
  self.value(value)
}

///|
fn ConfigFmt::map(
  self : Self,
  fields : Vector[(String, @moon_config.Ast)],
) -> Doc {
  ignore(self.token(TK_LBRACE))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, field in fields.to_array() {
    let (key, value) = field
    let field_doc = self.map_field(key, value)
    let after = self.placeholder()
    if !config_is_virtual(value) {
      ignore(self.checkpoint(value.loc().end))
    }
    ignore(self.token(TK_COMMA))
    let separator_comments = self.placeholder()
    let printed_comma = if index + 1 < fields.length() {
      comma
    } else {
      switch(empty, comma)
    }
    let following_line = if index + 1 < fields.length() { line } else { empty }
    docs.push(
      field_doc + after + printed_comma + separator_comments + following_line,
    )
  }
  ignore(self.token(TK_RBRACE))
  let body = opening_comments + docs.fold(init=empty, Doc::add)
  if fields.is_empty() {
    char('{') + body + char('}')
  } else {
    group(char('{') + nest(line + body) + line + char('}'))
  }
}

///|
fn ConfigFmt::apply_field(
  self : Self,
  key : String,
  value : @moon_config.Ast,
) -> Doc {
  match value {
    Obj(fields, ..) => {
      ignore(self.token(TK_LPAREN))
      let opening_comments = self.placeholder()
      let docs : Array[Doc] = []
      for index, field in fields.to_array() {
        let (label, arg) = field
        self.consume_label()
        ignore(self.token(TK_COLON))
        let before_value = self.placeholder()
        if !config_is_virtual(arg) {
          ignore(self.checkpoint(arg.loc().start))
        }
        let arg_doc = config_label(label, self.migration) +
          colon +
          space +
          before_value +
          self.value(arg)
        let after = self.placeholder()
        if !config_is_virtual(arg) {
          ignore(self.checkpoint(arg.loc().end))
        }
        ignore(self.token(TK_COMMA))
        let separator_comments = self.placeholder()
        let printed_comma = if index + 1 < fields.length() {
          comma
        } else {
          switch(empty, comma)
        }
        let following_line = if index + 1 < fields.length() {
          line
        } else {
          empty
        }
        docs.push(
          arg_doc + after + printed_comma + separator_comments + following_line,
        )
      }
      ignore(self.token(TK_RPAREN))
      let body = opening_comments + docs.fold(init=empty, Doc::add)
      let args = if fields.is_empty() {
        char('(') + body + char(')')
      } else {
        group(char('(') + nest(softline + body) + softline + char(')'))
      }
      text(key) + args
    }
    _ => text(key) + text("()")
  }
}

///|
fn import_item_parts(item : @moon_config.Ast) -> (String, String?) {
  match item {
    Str(path, ..) => (path, None)
    Obj(fields, ..) => {
      let mut path = ""
      let mut package_alias = None
      for field in fields {
        let (key, value) = field
        match (key, value) {
          ("path", Str(value, ..)) => path = value
          ("alias", Str(value, ..)) => package_alias = Some(value)
          _ => ()
        }
      }
      (path, package_alias)
    }
    _ => ("", None)
  }
}

///|
fn ConfigFmt::import_item(self : Self, item : @moon_config.Ast) -> Doc {
  ignore(self.token(TK_STRING))
  let (path, package_alias) = import_item_parts(item)
  let path_doc = text(config_string_literal(path, self.migration))
  match package_alias {
    Some(package_alias) => {
      ignore(self.token(TK_PACKAGE_NAME))
      path_doc + space + text("@" + package_alias)
    }
    None => path_doc
  }
}

///|
fn ConfigFmt::import_block(
  self : Self,
  kind : String,
  value : @moon_config.Ast,
) -> Doc {
  match value {
    Arr(items, ..) => {
      ignore(self.token(TK_IMPORT))
      ignore(self.token(TK_LBRACE))
      let opening_comments = self.placeholder()
      let docs : Array[Doc] = []
      for index, item in items.to_array() {
        if !config_is_virtual(item) {
          ignore(self.checkpoint(item.loc().start))
        }
        let item_doc = self.import_item(item)
        let after = self.placeholder()
        if !config_is_virtual(item) {
          ignore(self.checkpoint(item.loc().end))
        }
        ignore(self.token(TK_COMMA))
        let separator_comments = self.placeholder()
        let following_line = if index + 1 < items.length() {
          hardline
        } else {
          empty
        }
        docs.push(
          item_doc + after + comma + separator_comments + following_line,
        )
      }
      ignore(self.token(TK_RBRACE))
      let body = opening_comments + docs.fold(init=empty, Doc::add)
      let imports = char('{') + nest(hardline + body) + hardline + char('}')
      let suffix = match kind {
        "test-import" => {
          ignore(self.token(TK_FOR))
          ignore(self.token(TK_STRING))
          space + text("for \"test\"")
        }
        "wbtest-import" => {
          ignore(self.token(TK_FOR))
          ignore(self.token(TK_STRING))
          space + text("for \"wbtest\"")
        }
        _ => empty
      }
      text("import") + space + imports + suffix
    }
    _ => text("import {}")
  }
}

///|
fn dependency_spec(name : String, value : @moon_config.Ast) -> String {
  match value {
    Str(version, ..) if version != "" => name + "@" + version
    _ => name
  }
}

///|
fn ConfigFmt::dependency_block(self : Self, value : @moon_config.Ast) -> Doc {
  match value {
    Obj(deps, ..) => {
      ignore(self.token(TK_IMPORT))
      ignore(self.token(TK_LBRACE))
      let opening_comments = self.placeholder()
      let docs : Array[Doc] = []
      for index, dependency in deps.to_array() {
        let (name, version) = dependency
        if !config_is_virtual(version) {
          ignore(self.checkpoint(version.loc().start))
          ignore(self.token(TK_STRING))
        }
        let item_doc = text(
          config_string_literal(dependency_spec(name, version), self.migration),
        )
        let after = self.placeholder()
        if !config_is_virtual(version) {
          ignore(self.checkpoint(version.loc().end))
        }
        ignore(self.token(TK_COMMA))
        let separator_comments = self.placeholder()
        let following_line = if index + 1 < deps.length() {
          hardline
        } else {
          empty
        }
        docs.push(
          item_doc + after + comma + separator_comments + following_line,
        )
      }
      ignore(self.token(TK_RBRACE))
      let body = opening_comments + docs.fold(init=empty, Doc::add)
      if deps.is_empty() {
        empty
      } else {
        text("import") +
        space +
        char('{') +
        nest(hardline + body) +
        hardline +
        char('}')
      }
    }
    _ => empty
  }
}

///|
fn config_pkg_known(key : String) -> Bool {
  match key {
    "import"
    | "test-import"
    | "wbtest-import"
    | "warn-list"
    | "supported-targets"
    | "pkgtype"
    | "dev_build"
    | "formatter"
    | "rule" => true
    _ => false
  }
}

///|
fn config_mod_known(key : String) -> Bool {
  match key {
    "name"
    | "version"
    | "deps"
    | "warn-list"
    | "readme"
    | "repository"
    | "license"
    | "keywords"
    | "description"
    | "source"
    | "supported-targets"
    | "preferred-target"
    | "rule" => true
    _ => false
  }
}

///|
fn config_known(kind : ConfigKind, key : String) -> Bool {
  match kind {
    MoonPkg => config_pkg_known(key)
    MoonMod => config_mod_known(key)
  }
}

///|
fn ConfigFmt::assignment(
  self : Self,
  source_name : String,
  value : @moon_config.Ast,
) -> Doc {
  text(source_name) + space + equal + space + self.value(value)
}

///|
fn ConfigFmt::known_field(
  self : Self,
  kind : ConfigKind,
  key : String,
  value : @moon_config.Ast,
) -> Doc {
  match (kind, key) {
    (MoonPkg, "import" | "test-import" | "wbtest-import") =>
      self.import_block(key, value)
    (MoonPkg, "warn-list") => self.assignment("warnings", value)
    (MoonPkg, "supported-targets") =>
      self.assignment("supported_targets", value)
    (MoonPkg, "pkgtype" | "dev_build" | "formatter" | "rule") =>
      self.apply_field(key, value)
    (MoonMod, "deps") => self.dependency_block(value)
    (MoonMod, "warn-list") => self.assignment("warnings", value)
    (MoonMod, "supported-targets") =>
      self.assignment("supported_targets", value)
    (MoonMod, "preferred-target") => self.assignment("preferred_target", value)
    (MoonMod, "rule") => self.apply_field(key, value)
    (_, key) => self.assignment(key, value)
  }
}

///|
fn ConfigFmt::option_field(
  self : Self,
  key : String,
  value : @moon_config.Ast,
) -> Doc {
  self.consume_label()
  ignore(self.token(TK_COLON))
  let before_value = self.placeholder()
  if !config_is_virtual(value) {
    ignore(self.checkpoint(value.loc().start))
  }
  config_label(key, self.migration) +
  colon +
  space +
  before_value +
  self.value(value)
}

///|
fn ConfigFmt::root_field(
  self : Self,
  kind : ConfigKind,
  key : String,
  value : @moon_config.Ast,
) -> (Bool, Doc) {
  let before = self.placeholder()
  if !config_is_virtual(value) {
    ignore(self.checkpoint(value.loc().start))
  }
  let doc = if config_known(kind, key) {
    self.known_field(kind, key, value)
  } else {
    self.option_field(key, value)
  }
  let after = self.placeholder()
  if !config_is_virtual(value) {
    ignore(self.checkpoint(value.loc().end))
  }
  (config_known(kind, key), before + doc + after)
}

///|
fn join_config_sections(sections : Array[Doc]) -> Doc {
  let mut result = empty
  for index, section in sections {
    if index > 0 {
      result += hardline + hardline
    }
    result += section
  }
  result
}

///|
fn config_options_doc(fields : Array[Doc]) -> Doc {
  if fields.is_empty() {
    empty
  } else {
    let mut body = empty
    for index, field in fields {
      if index > 0 {
        body += hardline
      }
      body += field + comma
    }
    text("options") + char('(') + nest(hardline + body) + hardline + char(')')
  }
}

///|
fn ConfigFmt::document(
  self : Self,
  kind : ConfigKind,
  ast : @moon_config.Ast,
) -> Doc {
  let initial = self.placeholder()
  let known_sections : Array[Doc] = []
  let option_fields : Array[Doc] = []
  match ast {
    Obj(fields, ..) =>
      for field in fields {
        let (key, value) = field
        let (known, doc) = self.root_field(kind, key, value)
        if known {
          // Empty dependency sets have no source-level section.
          if !(kind is MoonMod &&
            key == "deps" &&
            value is Obj(deps, ..) &&
            deps.is_empty()) {
            known_sections.push(doc)
          }
        } else {
          option_fields.push(doc)
        }
      }
    _ => ()
  }
  if !option_fields.is_empty() {
    known_sections.push(config_options_doc(option_fields))
  }
  let final_comments = self.placeholder()
  initial + join_config_sections(known_sections) + final_comments + self.eof()
}

///|
fn config_format_once(
  ast : @moon_config.Ast,
  tokens : @tokens.Triples,
  kind : ConfigKind,
) -> String? {
  let mapper = @comment.CheckpointMapper::new(tokens)
  let attach = ConfigFmt::new(mapper~)
  ignore(attach.document(kind, ast))
  mapper.begin_pretty()
  let pretty = ConfigFmt::new(mapper~)
  let doc = pretty.document(kind, ast)
  guard mapper.is_complete() else { return None }
  Some(@pp.render(doc, width=80).trim_end().to_owned())
}

///|
fn config_comment_contents(tokens : @tokens.Triples) -> Array[String] {
  let result = []
  for triple in tokens {
    match triple.0 {
      COMMENT(comment) if !comment.consumed_by_docstring =>
        result.push(comment.content)
      _ => ()
    }
  }
  result
}

///|
fn parse_config(source : String, kind : ConfigKind) -> ConfigParseResult? {
  let lexed = @lexer.tokens_from_string(comment=true, source)
  if !lexed.errors.is_empty() {
    return None
  }
  let (ast, reports) = match kind {
    MoonPkg => @moon_config.parse_moon_pkg(name="", source)
    MoonMod => @moon_config.parse_moon_mod(name="", source)
  }
  if !reports.is_empty() {
    return None
  }
  Some({
    ast,
    tokens: lexed.tokens,
    ordinary_comments: config_comment_contents(lexed.tokens),
  })
}

///|
fn config_value_equal(
  left : @moon_config.Ast,
  right : @moon_config.Ast,
) -> Bool {
  match (left, right) {
    (Null(_), Null(_)) => true
    (Bool(a, ..), Bool(b, ..)) => a == b
    (Str(a, ..), Str(b, ..)) => a == b
    (Float(a, ..), Float(b, ..)) => a == b
    (Arr(a, ..), Arr(b, ..)) => {
      if a.length() != b.length() {
        return false
      }
      for index = 0; index < a.length(); index = index + 1 {
        if !config_value_equal(a[index], b[index]) {
          return false
        }
      }
      true
    }
    (Obj(a, ..), Obj(b, ..)) => config_fields_equal(a, b)
    _ => false
  }
}

///|
fn config_fields_equal(
  left : Vector[(String, @moon_config.Ast)],
  right : Vector[(String, @moon_config.Ast)],
) -> Bool {
  if left.length() != right.length() {
    return false
  }
  for index = 0; index < left.length(); index = index + 1 {
    let (left_key, left_value) = left[index]
    let (right_key, right_value) = right[index]
    if left_key != right_key || !config_value_equal(left_value, right_value) {
      return false
    }
  }
  true
}

///|
fn canonical_config_fields(
  ast : @moon_config.Ast,
  kind : ConfigKind,
) -> Vector[(String, @moon_config.Ast)]? {
  match ast {
    Obj(fields, ..) => {
      let known = []
      let options = []
      for field in fields {
        let (key, value) = field
        if config_known(kind, key) {
          if !(kind is MoonMod &&
            key == "deps" &&
            value is Obj(deps, ..) &&
            deps.is_empty()) {
            known.push((key, value))
          }
        } else {
          options.push((key, value))
        }
      }
      known.push_iter(options.iter())
      Some(Vector(known))
    }
    _ => None
  }
}

///|
fn same_config_semantics(
  expected : @moon_config.Ast,
  actual : @moon_config.Ast,
  kind : ConfigKind,
) -> Bool {
  match
    (
      canonical_config_fields(expected, kind),
      canonical_config_fields(actual, kind),
    ) {
    (Some(left), Some(right)) => config_fields_equal(left, right)
    _ => false
  }
}

///|
fn split_dependency_spec(spec : String) -> (String, String) {
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
fn dependency_name_in(specs : ArrayView[String], name : String) -> Bool {
  for spec in specs {
    if split_dependency_spec(spec).0 == name {
      return true
    }
  }
  false
}

///|
fn edit_dependency_fields(
  fields : Vector[(String, @moon_config.Ast)],
  add_dependencies : ArrayView[String],
  remove_dependencies : ArrayView[String],
  removed_locs : Array[Location],
  surviving_locs : Array[Location],
) -> Vector[(String, @moon_config.Ast)] {
  let result : Array[(String, @moon_config.Ast)] = []
  for field in fields {
    let (name, value) = field
    if dependency_name_in(remove_dependencies, name) {
      removed_locs.push(value.loc())
    } else {
      surviving_locs.push(value.loc())
      result.push((name, value))
    }
  }
  for spec in add_dependencies {
    let (name, version) = split_dependency_spec(spec)
    let mut updated = false
    for index = 0; index < result.length(); index = index + 1 {
      let (existing_name, existing_value) = result[index]
      if existing_name == name {
        result[index] = (name, Str(version, loc=existing_value.loc()))
        updated = true
        break
      }
    }
    if !updated {
      result.push((name, Str(version, loc=config_dummy_location())))
    }
  }
  Vector(result)
}

///|
fn apply_dependency_edits_ordered(
  ast : @moon_config.Ast,
  add_dependencies : ArrayView[String],
  remove_dependencies : ArrayView[String],
) -> (@moon_config.Ast, Array[Location], Array[Location]) {
  let removed_locs = []
  let surviving_locs = []
  guard ast is Obj(fields, loc~) else {
    return (ast, removed_locs, surviving_locs)
  }
  let result : Array[(String, @moon_config.Ast)] = []
  let mut found_deps = false
  for field in fields {
    let (key, value) = field
    if key == "deps" && value is Obj(deps, loc=deps_loc) {
      found_deps = true
      let edited = edit_dependency_fields(
        deps, add_dependencies, remove_dependencies, removed_locs, surviving_locs,
      )
      if !edited.is_empty() {
        result.push(("deps", Obj(edited, loc=deps_loc)))
      }
    } else {
      result.push((key, value))
    }
  }
  if !found_deps && !add_dependencies.is_empty() {
    let deps : Array[(String, @moon_config.Ast)] = []
    for spec in add_dependencies {
      let (name, version) = split_dependency_spec(spec)
      deps.push((name, Str(version, loc=config_dummy_location())))
    }
    result.push(("deps", Obj(Vector(deps), loc=config_dummy_location())))
  }
  (Obj(Vector(result), loc~), removed_locs, surviving_locs)
}

///|
fn is_removed_trailing_comment(
  token : @tokens.Triple,
  removed_locs : Array[Location],
  surviving_locs : Array[Location],
) -> Bool {
  match token {
    (COMMENT(comment), start, _) if comment.kind is InlineTrailing => {
      let mut nearest_end = -1
      let mut nearest_was_removed = false
      for loc in removed_locs {
        if start.lnum == loc.end.lnum &&
          start.cnum >= loc.end.cnum &&
          loc.end.cnum > nearest_end {
          nearest_end = loc.end.cnum
          nearest_was_removed = true
        }
      }
      for loc in surviving_locs {
        if start.lnum == loc.end.lnum &&
          start.cnum >= loc.end.cnum &&
          loc.end.cnum > nearest_end {
          nearest_end = loc.end.cnum
          nearest_was_removed = false
        }
      }
      nearest_was_removed
    }
    _ => false
  }
}

///|
fn config_tokens_after_edits(
  tokens : @tokens.Triples,
  removed_locs : Array[Location],
  surviving_locs : Array[Location],
) -> @tokens.Triples {
  if removed_locs.is_empty() {
    return tokens
  }
  let result = []
  for token in tokens {
    if !is_removed_trailing_comment(token, removed_locs, surviving_locs) {
      result.push(token)
    }
  }
  result
}

///|
fn format_config_checked(
  source : String,
  kind : ConfigKind,
  add_dependencies? : ArrayView[String] = [],
  remove_dependencies? : ArrayView[String] = [],
) -> String {
  guard parse_config(source, kind) is Some(parsed) else { return source }
  let (expected_ast, removed_locs, surviving_locs) = if kind is MoonMod {
    apply_dependency_edits_ordered(
      parsed.ast,
      add_dependencies,
      remove_dependencies,
    )
  } else {
    (parsed.ast, [], [])
  }
  let tokens = config_tokens_after_edits(
    parsed.tokens,
    removed_locs,
    surviving_locs,
  )
  let expected_comments = config_comment_contents(tokens)
  guard config_format_once(expected_ast, tokens, kind) is Some(candidate) else {
    return source
  }
  guard parse_config(candidate, kind) is Some(reparsed) else { return source }
  guard same_config_semantics(expected_ast, reparsed.ast, kind) &&
    expected_comments == reparsed.ordinary_comments else {
    return source
  }
  guard config_format_once(reparsed.ast, reparsed.tokens, kind) is Some(second) &&
    second == candidate else {
    return source
  }
  candidate
}

///|
fn json_to_config_ast(value : Json) -> @moon_config.Ast {
  let loc = config_dummy_location()
  match value {
    Null => Null(loc~)
    True => Bool(true, loc~)
    False => Bool(false, loc~)
    String(value) => Str(value, loc~)
    Number(number, repr~) => Float(repr.unwrap_or(number.to_string()), loc~)
    Array(items) => Arr(Vector(items.map(json_to_config_ast)), loc~)
    Object(fields) => {
      let ordered = []
      for key, value in fields {
        ordered.push((key, json_to_config_ast(value)))
      }
      Obj(Vector(ordered), loc~)
    }
  }
}

///|
fn render_migration(ast : @moon_config.Ast, kind : ConfigKind) -> String {
  ConfigFmt::new(migration=true).document(kind, ast)
  |> @pp.render(width=80)
  |> String::trim_end
  |> StringView::to_owned
}

///|
pub fn format_moon_pkg(source : String) -> String {
  format_config_checked(source, MoonPkg)
}

///|
pub fn format_moon_mod(
  source : String,
  add_dependencies? : ArrayView[String] = [],
  remove_dependencies? : ArrayView[String] = [],
) -> String {
  format_config_checked(
    source,
    MoonMod,
    add_dependencies~,
    remove_dependencies~,
  )
}

///|
pub fn format_json_to_moon_pkg(source : String) -> String raise {
  @json.parse(source) |> json_to_config_ast |> render_migration(MoonPkg)
}

///|
pub fn format_json_to_moon_mod(source : String) -> String raise {
  @json.parse(source) |> json_to_config_ast |> render_migration(MoonMod)
}
