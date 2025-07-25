///|
test "map fn init to top expr" {
  let loc = no_location
  let fun_decl = @syntax.FunDecl::{
    type_name: None,
    name: Binder::{ name: "init", loc },
    has_error: None,
    is_async: false,
    decl_params: None,
    params_loc: dummy_loc,
    quantifiers: @list.of([]),
    return_type: None,
    error_type: @syntax.NoErrorType,
    vis: @syntax.Default,
    attrs: @list.of([]),
    doc: dummy_docstring,
  }
  let func_def = Impl::TopFuncDef(
    fun_decl~,
    decl_body=DeclBody(
      expr=Expr::Constant(c=Int("42"), loc~),
      local_types=@list.empty(),
    ),
    loc=no_location,
  )
  @json.inspect(post_process(@list.singleton(func_def)), content=[
    {
      "type": "Impl::TopExpr",
      "expr": {
        "type": "Expr::Constant",
        "c": { "type": "Constant::Int", "0": "42" },
        "loc": null,
      },
      "is_main": false,
      "local_types": [],
      "loc": null,
    },
  ])
}
