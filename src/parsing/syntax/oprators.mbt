///|
priv enum OperatorKind {
  Prefix
  Infix(should_have_equal_param_type~ : Bool)
  Mixfix(arity~ : Int, snippet~ : String)
}

///|
priv enum OperatorImpl {
  RegularFunction
  Method
  TraitMethod(@basic.TypePath)
}

///|
priv struct OperatorInfo {
  op_name : String
  method_name : String
  kind : OperatorKind
  impl_ : OperatorImpl
}

///|
let op_equal_info : OperatorInfo = OperatorInfo::{
  op_name: "==",
  method_name: "op_equal",
  kind: Infix(should_have_equal_param_type=true),
  impl_: TraitMethod(@basic.trait_eq),
}

///|
let op_add_info : OperatorInfo = OperatorInfo::{
  op_name: "+",
  method_name: "op_add",
  kind: Infix(should_have_equal_param_type=true),
  impl_: TraitMethod(@basic.trait_add),
}

///|
let op_get_info : OperatorInfo = OperatorInfo::{
  op_name: "_[_]",
  method_name: "op_get",
  kind: Mixfix(arity=2, snippet="[$1]$0"),
  impl_: Method,
}

///|
let op_get_slice_info : OperatorInfo = OperatorInfo::{
  op_name: "_[_:_]",
  method_name: "op_as_view",
  kind: Mixfix(arity=3, snippet="[$1:$2]$0"),
  impl_: Method,
}

///|
let op_set_info : OperatorInfo = OperatorInfo::{
  op_name: "_[_]=_",
  method_name: "op_set",
  kind: Mixfix(arity=3, snippet="[$1] = $0"),
  impl_: Method,
}

///| 
/// [op_bitwise_and] is not mixfix, but IDE's dot completion will filter out infix operator.
/// So we make it mixfix to make it appear in the completion list.
let op_bitwise_and : OperatorInfo = OperatorInfo::{
  op_name: "&",
  method_name: "land",
  kind: Mixfix(arity=2, snippet=".land($1)"),
  impl_: TraitMethod(@basic.trait_bit_and),
}

///| 
/// [op_bitwise_xor] is not mixfix, but IDE's dot completion will filter out infix operator.
/// So we make it mixfix to make it appear in the completion list.
let op_bitwise_xor : OperatorInfo = OperatorInfo::{
  op_name: "^",
  method_name: "lxor",
  kind: Mixfix(arity=2, snippet=".lxor($1)"),
  impl_: TraitMethod(@basic.trait_bit_xor),
}

///| 
/// [op_bitwise_or] is not mixfix, but IDE's dot completion will filter out infix operator.
/// So we make it mixfix to make it appear in the completion list.
let op_bitwise_or : OperatorInfo = OperatorInfo::{
  op_name: "|",
  method_name: "lor",
  kind: Mixfix(arity=2, snippet=".lor($1)"),
  impl_: TraitMethod(@basic.trait_bit_or),
}

///|
let op_bitwise_leftshift : OperatorInfo = OperatorInfo::{
  op_name: "<<",
  method_name: "op_shl",
  kind: Infix(should_have_equal_param_type=false),
  impl_: TraitMethod(@basic.trait_shl),
}

///|
let op_bitwise_rightshift : OperatorInfo = OperatorInfo::{
  op_name: ">>",
  method_name: "op_shr",
  kind: Infix(should_have_equal_param_type=false),
  impl_: TraitMethod(@basic.trait_shr),
}

///|
let operators : Array[OperatorInfo] = [
  OperatorInfo::{
    op_name: "~-",
    method_name: "op_neg",
    kind: Prefix,
    impl_: TraitMethod(@basic.trait_neg),
  },
  op_add_info,
  OperatorInfo::{
    op_name: "-",
    method_name: "op_sub",
    kind: Infix(should_have_equal_param_type=true),
    impl_: TraitMethod(@basic.trait_sub),
  },
  OperatorInfo::{
    op_name: "*",
    method_name: "op_mul",
    kind: Infix(should_have_equal_param_type=true),
    impl_: TraitMethod(@basic.trait_mul),
  },
  OperatorInfo::{
    op_name: "/",
    method_name: "op_div",
    kind: Infix(should_have_equal_param_type=true),
    impl_: TraitMethod(@basic.trait_div),
  },
  OperatorInfo::{
    op_name: "%",
    method_name: "op_mod",
    kind: Infix(should_have_equal_param_type=false),
    impl_: TraitMethod(@basic.trait_mod),
  },
  op_get_info,
  op_get_slice_info,
  op_set_info,
  op_equal_info,
  OperatorInfo::{
    op_name: "!=",
    method_name: "op_notequal",
    kind: Infix(should_have_equal_param_type=true),
    impl_: RegularFunction,
  },
  OperatorInfo::{
    op_name: "<",
    method_name: "op_lt",
    kind: Infix(should_have_equal_param_type=true),
    impl_: RegularFunction,
  },
  OperatorInfo::{
    op_name: "<=",
    method_name: "op_le",
    kind: Infix(should_have_equal_param_type=true),
    impl_: RegularFunction,
  },
  OperatorInfo::{
    op_name: ">",
    method_name: "op_gt",
    kind: Infix(should_have_equal_param_type=true),
    impl_: RegularFunction,
  },
  OperatorInfo::{
    op_name: ">=",
    method_name: "op_ge",
    kind: Infix(should_have_equal_param_type=true),
    impl_: RegularFunction,
  },
  op_bitwise_xor,
  op_bitwise_or,
  op_bitwise_and,
  op_bitwise_leftshift,
  op_bitwise_rightshift,
  // "&&" and "||" are not methods, due to their short-circuit nature.
  // "!" is also not a method for consistency with boolean operators.
]

///|
let operators_table : @hashmap.T[String, String] = {
  let table = @hashmap.new(capacity=operators.length() * 4)
  for op in operators {
    table[op.op_name] = op.method_name
  }
  table
}
