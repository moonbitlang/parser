///|
// Parser-local declaration shapes are available before constructing the read-only
// NodeKind. Each shape is selected together with its dedicated constructor.
priv enum TopItemShape {
  Other
  Using
  Function
  TypeAlias
  TypeAbstract
  TupleStruct
  RecordStruct
  SuberrorEnum
  Suberror
  ExtendEnum
  ExtensibleEnum
  EnumVariant
  EnumView
  Trait
  ImplMethod
  ImplRelation
}

///|
fn classify_top_item_construction(
  children : Array[(String?, CstNode)],
  parsed_kind : TopItemKind,
  is_suberror : Bool,
  is_extenum : Bool,
  saw_body : Bool,
) -> (
  TopItemShape,
  (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) {
  match parsed_kind {
    TopItemKind::Invalid =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::error(loc~, source_span~, entries~)
        },
      )
    Import =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_import(loc~, source_span~, entries~)
        },
      )
    Extend =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_extend(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Using =>
      (
        Using,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_using(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Function =>
      (
        Function,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_function(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Let =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_let(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Const =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_const(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Type =>
      if top_item_children_have_label(children, "type") {
        (
          TypeAlias,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_type_alias(loc~, source_span~, entries~)
          },
        )
      } else {
        (
          TypeAbstract,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_type_abstract(loc~, source_span~, entries~)
          },
        )
      }
    TopItemKind::Struct =>
      if top_item_child_with_label_has_kind(children, "body", fn(kind) {
          kind is List_TupleStructTypeList
        }) {
        (
          TupleStruct,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_tuple_struct(loc~, source_span~, entries~)
          },
        )
      } else {
        (
          RecordStruct,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_record_struct(loc~, source_span~, entries~)
          },
        )
      }
    TopItemKind::Enum =>
      if is_suberror {
        if saw_body {
          (
            SuberrorEnum,
            fn(loc, source_span, children) {
              let entries = declaration_children(children)
              @node.CstNode::impl_suberror_enum(loc~, source_span~, entries~)
            },
          )
        } else {
          (
            Suberror,
            fn(loc, source_span, children) {
              let entries = declaration_children(children)
              @node.CstNode::impl_suberror(loc~, source_span~, entries~)
            },
          )
        }
      } else if is_extenum {
        if top_item_children_have_child_or_grandchild_kind(children, fn(kind) {
            kind is Operator_Augmented
          }) {
          (
            ExtendEnum,
            fn(loc, source_span, children) {
              let entries = declaration_children(children)
              @node.CstNode::impl_extend_enum(loc~, source_span~, entries~)
            },
          )
        } else {
          (
            ExtensibleEnum,
            fn(loc, source_span, children) {
              let entries = declaration_children(children)
              @node.CstNode::impl_extensible_enum(loc~, source_span~, entries~)
            },
          )
        }
      } else {
        (
          EnumVariant,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_enum_variant(loc~, source_span~, entries~)
          },
        )
      }
    TopItemKind::EnumView =>
      (
        EnumView,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_enum_view(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Trait =>
      (
        Trait,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_trait(loc~, source_span~, entries~)
        },
      )
    TopItemKind::Impl =>
      if top_item_children_have_label(children, "method") {
        (
          ImplMethod,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_impl_method(loc~, source_span~, entries~)
          },
        )
      } else {
        (
          ImplRelation,
          fn(loc, source_span, children) {
            let entries = declaration_children(children)
            @node.CstNode::impl_impl_relation(loc~, source_span~, entries~)
          },
        )
      }
    TopItemKind::Test =>
      (
        Other,
        fn(loc, source_span, children) {
          let entries = declaration_children(children)
          @node.CstNode::impl_test(loc~, source_span~, entries~)
        },
      )
  }
}

///|
fn type_desc_builder_for_shape(
  kind : TopItemShape,
) -> ((Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode)? {
  match kind {
    TypeAbstract =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_abstract(loc~, source_span~)
      })
    TypeAlias =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_alias(loc~, source_span~)
      })
    RecordStruct =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_record(loc~, source_span~)
      })
    TupleStruct =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_tuple_struct(loc~, source_span~)
      })
    EnumVariant =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_variant(loc~, source_span~)
      })
    ExtensibleEnum =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_extensible_enum(loc~, source_span~)
      })
    ExtendEnum =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_extend_enum(loc~, source_span~)
      })
    Suberror =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_error_no_payload(loc~, source_span~)
      })
    SuberrorEnum =>
      Some(fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::type_desc_error_enum_payload(loc~, source_span~)
      })
    _ => None
  }
}

///|
fn decl_body_builder(
  children : Array[(String?, CstNode)],
  is_declare : Bool,
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  if is_declare {
    return fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::decl_body_none(loc~, source_span~)
    }
  }
  if first_decl_body_child(children) is Some(marker) {
    return match marker.kind {
      DeclBody_None =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_none(loc~, source_span~)
        }
      DeclBody_Expr =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_expr(loc~, source_span~)
        }
      DeclBody_Import =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_import(loc~, source_span~)
        }
      DeclBody_EmbeddedString =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_embedded_string(loc~, source_span~)
        }
      DeclBody_EmbeddedMultilineString =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_embedded_multiline_string(loc~, source_span~)
        }
      _ =>
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_expr(loc~, source_span~)
        }
    }
  }
  let stubs = CstNode::children_with_label_from(children, "stub")
  match stubs.get(0) {
    Some(first) =>
      if first.kind is Stub_ModuleName {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_import(loc~, source_span~)
        }
      } else if stubs.length() > 1 {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_embedded_multiline_string(loc~, source_span~)
        }
      } else if first.kind is Stub_Code {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_embedded_string(loc~, source_span~)
        }
      } else {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::decl_body_expr(loc~, source_span~)
        }
      }
    None =>
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::decl_body_expr(loc~, source_span~)
      }
  }
}

///|
fn first_decl_body_child(children : Array[(String?, CstNode)]) -> CstNode? {
  for (_, child) in children {
    if child.kind.is_decl_body() {
      return Some(child)
    }
  }
  None
}

///|
fn prepare_decl_body(
  children : Array[(String?, CstNode)],
  loc : Location,
  source_span : (Int, Int),
  is_declare : Bool,
) -> Array[(String?, CstNode)] {
  let body_builder = decl_body_builder(children, is_declare)
  let entries = children.filter(entry => !entry.1.kind.is_decl_body())
  push_marker_child(entries, loc, source_span, "decl_body_kind", body_builder)
  entries
}
