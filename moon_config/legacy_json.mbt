///|
pub impl ToJson for Ast with fn to_json(self) {
  fn convert(ast : Ast, fold_duplicates : Bool) -> Json {
    match ast {
      Null(_) => Json::null()
      Bool(value, ..) => Json::boolean(value)
      Str(str, ..) => Json::string(str)
      Float(float, ..) => Json::number(0.0, repr=float)
      Arr(content, ..) =>
        Json::array(content.to_array().map(item => convert(item, false)))
      Obj(fields, ..) => {
        if fold_duplicates {
          let groups : Map[String, Array[Json]] = Map([])
          for field in fields {
            let (key, value) = field
            let item = convert(value, false)
            match groups.get(key) {
              Some(items) => {
                items.push(item)
                groups[key] = items
              }
              None => groups[key] = [item]
            }
          }
          let object : Map[String, Json] = Map([])
          groups.each(fn(key, items) {
            object[key] = if items.length() == 1 {
              items[0]
            } else {
              Json::array(items)
            }
          })
          return Json::object(object)
        }
        let object : Map[String, Json] = Map([])
        for field in fields {
          let (key, value) = field
          object[key] = convert(value, false)
        }
        Json::object(object)
      }
    }
  }

  convert(self, true)
}
