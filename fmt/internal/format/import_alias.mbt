///|
fn default_import_alias(path : String) -> String {
  match path.rev_split_once("/") {
    Some((_, alias_)) => alias_.to_owned()
    None => path
  }
}

///|
fn canonical_import_alias(path : String, alias_ : String?) -> String? {
  match alias_ {
    None => None
    Some(alias_) => {
      let import_all = alias_.has_prefix("*")
      let name = if import_all { alias_[1:].to_owned() } else { alias_ }
      if name == default_import_alias(path) {
        if import_all {
          Some("*")
        } else {
          None
        }
      } else {
        Some(alias_)
      }
    }
  }
}
