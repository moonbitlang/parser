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
}

///|
fn ConfigFmt::new(mapper? : @comment.CheckpointMapper) -> Self {
  { mapper, }
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
  { fname: "", lnum: -1, bol: 0, cnum: 0, }
}

///|
fn config_dummy_location() -> Location {
  let pos = config_dummy_position()
  { start: pos, end: pos, }
}

///|
fn config_is_virtual(ast : @moon_config.Expr) -> Bool {
  ast.loc().start.lnum < 0
}

///|
fn config_string_literal(value : String) -> String {
  "\"" + value + "\""
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
fn config_label(label : String) -> Doc {
  if config_label_is_plain(label) {
    text(label)
  } else {
    text(config_string_literal(label))
  }
}

///|
fn config_map_key(label : String) -> Doc {
  text(config_string_literal(label))
}

///|
fn ConfigFmt::consume_label(self : Self) -> Unit {
  // Both spellings are accepted for labels. Trying the other spelling after a
  // successful match is harmless because the cursor is then at `:`.
  ignore(self.token(TK_LIDENT))
  ignore(self.token(TK_STRING))
}

///|
fn ConfigFmt::value(self : Self, ast : @moon_config.Expr) -> Doc {
  match ast {
    Hole(_) => text("\"\"")
    Bool(value~, ..) => text(if value { "true" } else { "false" })
    String(str=value, ..) => text(config_string_literal(value))
    Int(repr=value, ..) => text(value)
    Array(items~, ..) => self.array(items)
    Map(fields~, ..) => self.map(fields)
    Apply(name~, args~, ..) => self.apply_field(name, args)
  }
}

///|
fn ConfigFmt::array(self : Self, items : Array[@moon_config.Expr]) -> Doc {
  ignore(self.token(TK_LBRACKET))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, item in items {
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
  value : @moon_config.Expr,
) -> Doc {
  ignore(self.token(TK_STRING))
  ignore(self.token(TK_COLON))
  let before_value = self.placeholder()
  if !config_is_virtual(value) {
    ignore(self.checkpoint(value.loc().start))
  }
  config_map_key(key) + colon + space + before_value + self.value(value)
}

///|
fn ConfigFmt::map(self : Self, fields : Array[@moon_config.Field]) -> Doc {
  ignore(self.token(TK_LBRACE))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, field in fields {
    let { key, value, .. } = field
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
  fields : Array[@moon_config.Field],
) -> Doc {
  ignore(self.token(TK_LPAREN))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, field in fields {
    let { key: label, value: arg, .. } = field
    self.consume_label()
    ignore(self.token(TK_COLON))
    let before_value = self.placeholder()
    if !config_is_virtual(arg) {
      ignore(self.checkpoint(arg.loc().start))
    }
    let arg_doc = config_label(label) +
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
    let following_line = if index + 1 < fields.length() { line } else { empty }
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

///|
fn ConfigFmt::import_item(self : Self, item : @moon_config.ImportItem) -> Doc {
  ignore(self.token(TK_STRING))
  let path = item.path
  let package_alias = item.alias_.map(pair => pair.0)
  let path_doc = text(config_string_literal(path))
  let package_alias = canonical_import_alias(path, package_alias)
  match package_alias {
    Some(package_alias) => {
      let import_all = package_alias.has_prefix("*")
      let alias_name = if import_all {
        package_alias[1:].to_owned()
      } else {
        package_alias
      }
      let alias_doc = if alias_name == "" {
        empty
      } else {
        ignore(self.token(TK_PACKAGE_NAME))
        space + text("@" + alias_name)
      }
      let all_doc = if import_all {
        ignore(self.token(TK_STAR))
        space + char('*')
      } else {
        empty
      }
      path_doc + alias_doc + all_doc
    }
    None => path_doc
  }
}

///|
fn ConfigFmt::import_block(
  self : Self,
  kind : @moon_config.ImportKind,
  items : Array[@moon_config.ImportItem],
) -> Doc {
  ignore(self.token(TK_IMPORT))
  ignore(self.token(TK_LBRACE))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for index, item in items {
    if item.loc.start.lnum >= 0 {
      ignore(self.checkpoint(item.loc.start))
    }
    let item_doc = self.import_item(item)
    let after = self.placeholder()
    if item.loc.start.lnum >= 0 {
      ignore(self.checkpoint(item.loc.end))
    }
    ignore(self.token(TK_COMMA))
    let separator_comments = self.placeholder()
    let following_line = if index + 1 < items.length() {
      hardline
    } else {
      empty
    }
    docs.push(item_doc + after + comma + separator_comments + following_line)
  }
  ignore(self.token(TK_RBRACE))
  let body = opening_comments + docs.fold(init=empty, Doc::add)
  let imports = char('{') + nest(hardline + body) + hardline + char('}')
  let suffix = match kind {
    Test => {
      ignore(self.token(TK_FOR))
      ignore(self.token(TK_STRING))
      space + text("for \"test\"")
    }
    Wbtest => {
      ignore(self.token(TK_FOR))
      ignore(self.token(TK_STRING))
      space + text("for \"wbtest\"")
    }
    Normal => empty
  }
  text("import") + space + imports + suffix
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
fn ConfigFmt::document(self : Self, ast : @moon_config.Ast) -> Doc {
  let initial = self.placeholder()
  let sections : Array[Doc] = []
  for statement in ast.0 {
    let loc : Location = match statement {
      Assign(field) =>
        { start: field.key_loc.start, end: field.value.loc().end, }
      Expr(expr) => expr.loc()
      Import(loc~, ..) => loc
    }
    let before = self.placeholder()
    if loc.start.lnum >= 0 {
      ignore(self.checkpoint(loc.start))
    }
    let doc = match statement {
      Assign(field) => {
        self.consume_label()
        ignore(self.token(TK_EQUAL))
        let before_value = self.placeholder()
        if !config_is_virtual(field.value) {
          ignore(self.checkpoint(field.value.loc().start))
        }
        text(field.key) +
        space +
        equal +
        space +
        before_value +
        self.value(field.value)
      }
      Expr(Apply(name="options", args~, ..)) => self.options(args)
      Expr(value) => {
        if value is Apply(_) {
          ignore(self.token(TK_LIDENT))
        }
        self.value(value)
      }
      Import(items~, kind~, ..) => self.import_block(kind, items)
    }
    let after = self.placeholder()
    if loc.end.lnum >= 0 {
      ignore(self.checkpoint(loc.end))
    }
    sections.push(before + doc + after)
  }
  let final_comments = self.placeholder()
  initial + join_config_sections(sections) + final_comments + self.eof()
}

///|
fn ConfigFmt::options(self : Self, fields : Array[@moon_config.Field]) -> Doc {
  ignore(self.token(TK_LIDENT))
  ignore(self.token(TK_LPAREN))
  let opening_comments = self.placeholder()
  let docs : Array[Doc] = []
  for field in fields {
    self.consume_label()
    ignore(self.token(TK_COLON))
    let before = self.placeholder()
    if !config_is_virtual(field.value) {
      ignore(self.checkpoint(field.value.loc().start))
    }
    let doc = config_label(field.key) +
      colon +
      space +
      before +
      self.value(field.value)
    let after = self.placeholder()
    if !config_is_virtual(field.value) {
      ignore(self.checkpoint(field.value.loc().end))
    }
    ignore(self.token(TK_COMMA))
    let separator_comments = self.placeholder()
    docs.push(doc + after + comma + separator_comments)
  }
  ignore(self.token(TK_RPAREN))
  let mut body = opening_comments
  for index, doc in docs {
    if index > 0 {
      body += hardline
    }
    body += doc
  }
  text("options(") + nest(hardline + body) + hardline + char(')')
}

///|
fn parse_config(source : String, kind : ConfigKind) -> ConfigParseResult? {
  let result = match kind {
    MoonPkg =>
      @moon_config.parse_moon_pkg(
        name="",
        keep_comments=true,
        validate=false,
        source,
      )
    MoonMod =>
      @moon_config.parse_moon_mod(
        name="",
        keep_comments=true,
        validate=false,
        source,
      )
  }
  if !result.diagnostics.errors.is_empty() {
    return None
  }
  Some({
    ast: result.ast,
    tokens: result.tokens,
    ordinary_comments: config_comment_contents(result.tokens),
  })
}

///|
fn config_value_without_locations(
  value : @moon_config.Expr,
) -> @moon_config.Expr {
  let loc = config_dummy_location()
  match value {
    Bool(value~, ..) => Bool(value~, loc~)
    Int(repr~, ..) => Int(repr~, loc~)
    String(str~, ..) => String(str~, loc~)
    Hole(_) => Hole(loc)
    Array(items~, ..) =>
      Array(items=items.map(config_value_without_locations), loc~)
    Map(fields~, ..) =>
      Map(fields=fields.map(config_field_without_locations), loc~)
    Apply(name~, args~, ..) =>
      Apply(
        name~,
        args=args.map(config_field_without_locations),
        name_loc=loc,
        args_loc=loc,
        loc~,
      )
  }
}

///|
fn config_field_without_locations(
  field : @moon_config.Field,
) -> @moon_config.Field {
  {
    key: field.key,
    key_loc: config_dummy_location(),
    value: config_value_without_locations(field.value),
  }
}

///|
fn config_without_locations(ast : @moon_config.Ast) -> @moon_config.Ast {
  let loc = config_dummy_location()
  @moon_config.Ast(
    ast.0.map(statement => {
      match statement {
        Assign(field) => Assign(config_field_without_locations(field))
        Expr(value) => Expr(config_value_without_locations(value))
        Import(items~, kind~, ..) =>
          Import(
            kind~,
            keyword_loc=loc,
            list_loc=loc,
            loc~,
            items=items.map(item => {
              path: item.path,
              path_loc: loc,
              loc,
              alias_: canonical_import_alias(
                item.path,
                item.alias_.map(pair => pair.0),
              ).map(name => (name, loc)),
            }),
          )
      }
    }),
  )
}

///|
fn same_config_semantics(
  expected : @moon_config.Ast,
  actual : @moon_config.Ast,
  _kind : ConfigKind,
) -> Bool {
  config_without_locations(expected) == config_without_locations(actual)
}

///|
fn config_format_once(
  ast : @moon_config.Ast,
  tokens : @tokens.Triples,
  _kind : ConfigKind,
) -> String? {
  let mapper = @comment.CheckpointMapper::new(tokens)
  let attach = ConfigFmt::new(mapper~)
  ignore(attach.document(ast))
  mapper.begin_pretty()
  let pretty = ConfigFmt::new(mapper~)
  let doc = pretty.document(ast)
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
fn edit_dependency_items(
  items : Array[@moon_config.ImportItem],
  add_dependencies : ArrayView[String],
  remove_dependencies : ArrayView[String],
  removed_locs : Array[Location],
  surviving_locs : Array[Location],
) -> Array[@moon_config.ImportItem] {
  let result = []
  for item in items {
    if dependency_name_in(
        remove_dependencies,
        split_dependency_spec(item.path).0,
      ) {
      removed_locs.push(item.loc)
    } else {
      surviving_locs.push(item.loc)
      result.push(item)
    }
  }
  for spec in add_dependencies {
    let name = split_dependency_spec(spec).0
    let mut updated = false
    for index, item in result {
      if split_dependency_spec(item.path).0 == name {
        result[index] = { ..item, path: spec, }
        updated = true
        break
      }
    }
    if !updated {
      let loc = config_dummy_location()
      result.push({ path: spec, path_loc: loc, alias_: None, loc, })
    }
  }
  result
}

///|
fn apply_dependency_edits_ordered(
  ast : @moon_config.Ast,
  add_dependencies : ArrayView[String],
  remove_dependencies : ArrayView[String],
) -> (@moon_config.Ast, Array[Location], Array[Location]) {
  if add_dependencies.is_empty() && remove_dependencies.is_empty() {
    return (ast, [], [])
  }
  let removed_locs = []
  let surviving_locs = []
  let result : Array[@moon_config.Statement] = []
  let mut found_deps = false
  for statement in ast.0 {
    match statement {
      Import(items~, kind=Normal, keyword_loc~, list_loc~, loc~) => {
        let edited = edit_dependency_items(
          items,
          if found_deps {
            []
          } else {
            add_dependencies
          },
          remove_dependencies,
          removed_locs,
          surviving_locs,
        )
        found_deps = true
        if !edited.is_empty() {
          result.push(
            Import(items=edited, kind=Normal, keyword_loc~, list_loc~, loc~),
          )
        }
      }
      _ => result.push(statement)
    }
  }
  if !found_deps && !add_dependencies.is_empty() {
    let loc = config_dummy_location()
    let items = edit_dependency_items(
      [],
      add_dependencies,
      [],
      removed_locs,
      surviving_locs,
    )
    result.push(
      Import(items~, kind=Normal, keyword_loc=loc, list_loc=loc, loc~),
    )
  }
  (@moon_config.Ast(@vector.Vector(result)), removed_locs, surviving_locs)
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
  add_dependencies~ : ArrayView[String] = [],
  remove_dependencies~ : ArrayView[String] = [],
) -> String {
  guard parse_config(source, kind) is Some(parsed) else { return source }
  let migrated = migrate_config(parsed.ast, kind)
  let (expected_ast, removed_locs, surviving_locs) = if kind is MoonMod {
    apply_dependency_edits_ordered(
      migrated, add_dependencies, remove_dependencies,
    )
  } else {
    (migrated, [], [])
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
fn render_migration(ast : @moon_config.Ast) -> String {
  ConfigFmt::new().document(ast)
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
  add_dependencies~ : ArrayView[String] = [],
  remove_dependencies~ : ArrayView[String] = [],
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
  let ast = migrate_json_config(@json.parse(source), MoonPkg)
  render_migration(migrate_config(ast, MoonPkg))
}

///|
pub fn format_json_to_moon_mod(source : String) -> String raise {
  let ast = migrate_json_config(@json.parse(source), MoonMod)
  render_migration(migrate_config(ast, MoonMod))
}
