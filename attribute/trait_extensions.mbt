///|
pub extend Id with Show::{to_string, output}

///|
pub extend Id with @moonbitlang/core/debug.Debug::{to_repr}

///|
pub extend Expr with @moonbitlang/core/debug.Debug::{to_repr}

///|
pub extend Prop with @moonbitlang/core/debug.Debug::{to_repr}

///|
pub extend Attribute with @moonbitlang/core/debug.Debug::{to_repr}

///|
pub extend Attribute with ToJson::{to_json}

///|
pub extend ParseError with @moonbitlang/core/debug.Debug::{to_repr}
