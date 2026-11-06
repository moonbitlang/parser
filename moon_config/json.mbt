///|
// Keep all position fields independently of @basic.show_loc's display setting.
fn location_to_json(loc : Location) -> Json {
  { "start": ToJson::to_json(loc.start), "end": ToJson::to_json(loc.end) }
}

///|
fn field_to_json(field : Field) -> Json {
  {
    "key": Json::string(field.key),
    "key_loc": location_to_json(field.key_loc),
    "value": expr_to_json(field.value),
  }
}

///|
fn expr_to_json(expr : Expr) -> Json {
  match expr {
    Bool(value~, loc~) =>
      {
        "$tag": "Bool",
        "value": Json::boolean(value),
        "loc": location_to_json(loc),
      }
    Int(repr~, loc~) =>
      {
        "$tag": "Int",
        "repr": Json::string(repr),
        "loc": location_to_json(loc),
      }
    String(str~, loc~) =>
      {
        "$tag": "String",
        "str": Json::string(str),
        "loc": location_to_json(loc),
      }
    Array(items~, loc~) =>
      {
        "$tag": "Array",
        "items": Json::array(items.map(expr_to_json)),
        "loc": location_to_json(loc),
      }
    Map(fields~, loc~) =>
      {
        "$tag": "Map",
        "fields": Json::array(fields.map(field_to_json)),
        "loc": location_to_json(loc),
      }
    Apply(name~, name_loc~, args~, args_loc~, loc~) =>
      {
        "$tag": "Apply",
        "name": Json::string(name),
        "name_loc": location_to_json(name_loc),
        "args": Json::array(args.map(field_to_json)),
        "args_loc": location_to_json(args_loc),
        "loc": location_to_json(loc),
      }
    Hole(loc) => { "$tag": "Hole", "0": location_to_json(loc) }
  }
}

///|
fn import_item_to_json(item : ImportItem) -> Json {
  {
    "path": Json::string(item.path),
    "path_loc": location_to_json(item.path_loc),
    "alias_": if item.alias_ is Some((name, loc)) {
      Json::array([Json::string(name), location_to_json(loc)])
    } else {
      Json::null()
    },
    "loc": location_to_json(item.loc),
  }
}

///|
fn statement_to_json(statement : Statement) -> Json {
  match statement {
    Assign(field) => { "$tag": "Assign", "0": field_to_json(field) }
    Expr(expr) => { "$tag": "Expr", "0": expr_to_json(expr) }
    Import(items~, kind~, conditions~, keyword_loc~, list_loc~, loc~) => {
      let kind = match kind {
        Normal => "Normal"
        Test => "Test"
        Wbtest => "Wbtest"
      }
      {
        "$tag": "Import",
        "items": Json::array(items.map(import_item_to_json)),
        "kind": { "$tag": Json::string(kind) },
        "conditions": Json::array(
          conditions.map(condition => {
            "raw": Json::string(condition.raw),
            "parsed": ToJson::to_json(condition.parsed),
            "loc": location_to_json(condition.loc),
          }),
        ),
        "keyword_loc": location_to_json(keyword_loc),
        "list_loc": location_to_json(list_loc),
        "loc": location_to_json(loc),
      }
    }
  }
}

///|
pub impl ToJson for Ast with fn to_json(self) {
  let calls : Map[String, Array[Expr]] = Map([])
  for statement in self.0 {
    if statement is Expr(Apply(name~, args~, args_loc~, ..)) {
      calls.get_or_init(name, () => []).push(Map(fields=args, loc=args_loc))
    }
  }
  let statements : Array[Json] = []
  for statement in self.0 {
    if statement is Expr(Apply(name~, name_loc~, ..)) {
      // Emit each group at its first occurrence, retaining call order.
      if calls.remove_and_get(name) is Some(items) {
        if items.length() > 1 {
          let loc : Location = {
            start: items[0].loc().start,
            end: items[items.length() - 1].loc().end,
          }
          statements.push(
            statement_to_json(
              Assign({
                key: name,
                key_loc: name_loc,
                value: Array(items~, loc~),
              }),
            ),
          )
        } else {
          statements.push(statement_to_json(statement))
        }
      }
    } else {
      statements.push(statement_to_json(statement))
    }
  }
  Json::array(statements)
}

///|
pub extend Ast with ToJson::{to_json}
