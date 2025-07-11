%start<unit> t

%token CHAR
%token INT
%token BYTE
%token BYTES
%token FLOAT
%token STRING
%token MULTILINE_STRING
%token MULTILINE_INTERP
%token INTERP
%token ATTRIBUTE
%token LIDENT
%token UIDENT
%token POST_LABEL
%token COMMENT
%token NEWLINE
%token INFIX1
%token INFIX2
%token INFIX3
%token INFIX4
%token AUGMENTED_ASSIGNMENT
%token EOF
%token FALSE
%token TRUE
%token PUB "pub"
%token PRIV "priv"
%token READONLY "readonly"
%token IMPORT "import"
%token EXTERN "extern"
%token BREAK "break"
%token CONTINUE "continue"
%token STRUCT "struct"
%token ENUM "enum"
%token TRAIT "trait"
%token DERIVE "derive"
%token IMPL "impl"
%token WITH "with"
%token RAISE "raise"
%token THROW "throw"
%token TRY "try"
%token CATCH "catch"
%token ASYNC "async"
%token TYPEALIAS "typealias"
%token TRAITALIAS "traitalias"
%token FNALIAS "fnalias"
%token EQUAL "="
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token MINUS "-"
%token QUESTION "?"
%token EXCLAMATION "!"
%token DOT_LIDENT
%token DOT_UIDENT
%token DOT_INT
%token DOT_LPAREN ".("
%token COLONCOLON "::"
%token COLON ":"
%token SEMI ";"
%token LBRACKET "["
%token PLUS "+"
%token RBRACKET "]"
%token UNDERSCORE "_"
%token BAR "|"
%token LBRACE "{"
%token RBRACE "}"
%token AMPERAMPER "&&"
%token AMPER "&"
%token CARET "^"
%token BARBAR "||"
%token PACKAGE_NAME
%token AS "as"
%token PIPE "|>"
%token ELSE "else"
%token FN "fn"
%token IF "if"
%token LET "let"
%token CONST "const"
%token MATCH "match"
%token MUTABLE "mut"
%token TYPE "type"
%token FAT_ARROW "=>"
%token THIN_ARROW "->"
%token WHILE "while"
%token RETURN "return"
%token DOTDOT ".."
%token RANGE_INCLUSIVE "..="
%token RANGE_EXCLUSIVE "..<"
%token ELLIPSIS "..."
%token TEST "test"
%token LOOP "loop"
%token GUARD "guard"
%token FOR "for"
%token IN "in"
%token IS "is"

%nonassoc prec_type
%nonassoc "?" "!"

%%

t
  : LIDENT STRING ioption(";") imports sigs EOF {}
  ;

imports
  :  {}
  | "import" "(" separated_nonempty_list(";", package_import) ")" ";" {}
  ;

package_import
  : STRING {}
  | STRING "as" LIDENT {}
  ;

sigs
  :  {}
  | sig_ {}
  | sig_ ";" sigs {}
  ;

sig_
  : func_sig {}
  | type_sig {}
  | alias_sig {}
  | trait_sig {}
  | impl_sig {}
  | const_sig {}
  | value_sig {}
  ;

const_sig
  : "const" uident ":" type_ "=" constant {}
  ;

value_sig
  : "let" lident ":" type_ {}
  ;

func_sig_no_attr
  : FN lident loption(type_params_with_constraints) delimited("(", separated_list(",", parameter), ")") "->" return_type {}
  ;

func_sig
  : func_sig_no_attr {}
  | nonempty_list(ATTRIBUTE) func_sig_no_attr {}
  ;

impl_method_sig
  : list(ATTRIBUTE) lident loption(type_params_with_constraints) delimited("(", separated_list(",", parameter), ")") "->" return_type {}
  ;

trait_method_sig
  : lident delimited("(", separated_list(",", trait_method_parameter), ")") "->" return_type option(eq_underscore) {}
  ;

%inline eq_underscore
  : "=" "_" {}
  ;

type_sig
  : vis "extern" "type" type_decl_name_with_params {}
  | vis "type" type_decl_name_with_params {}
  | vis "type" type_decl_name_with_params type_ {}
  | vis "type" "!" UIDENT option(type_) {}
  | vis "type" "!" UIDENT "{" separated_list(";", enum_constructor) "}" {}
  | vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" {}
  | vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" {}
  ;

impl_sig
  : "impl" type_params_with_constraints qualified_uident "for" type_ {}
  | "impl" qualified_uident "for" type_ {}
  | "impl" uident "{" separated_nonempty_list(";", impl_method_sig) "}" {}
  | "impl" uident "::" lident {}
  ;

trait_sig
  : vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" {}
  | vis "trait" uident {}
  ;

alias_sig
  : vis "typealias" type_decl_name_with_params "=" type_ {}
  | vis "traitalias" uident "=" qualified_uident {}
  ;

enum_constructor
  : UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) {}
  ;

%inline eq_tag
  : "=" INT {}
  ;

constructor_param
  : option("mut") type_ {}
  | option("mut") POST_LABEL ":" type_ {}
  ;

record_decl_field
  : option("mut") LIDENT ":" type_ {}
  ;

type_param_with_constraints
  : uident {}
  | uident ":" separated_nonempty_list("+", type_constraint) {}
  ;

type_params_with_constraints
  : "[" separated_list(",", type_param_with_constraints) "]" {}
  ;

type_param_no_constraints
  : uident {}
  | "_" {}
  ;

type_params_no_constraints
  : "[" separated_list(",", type_param_no_constraints) "]" {}
  ;

optional_type_params_no_constraints
  :  {}
  | type_params_no_constraints {}
  ;

type_constraint
  : qualified_uident {}
  ;

type_decl_name_with_params
  : uident optional_type_params_no_constraints {}
  ;

type_
  : type_ "?" {}
  | "(" type_ "," separated_nonempty_list(",", type_) ")" {}
  | is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type {}
  | is_async "(" ")" "->" return_type {}
  | "(" type_ ")" {}
  | is_async "(" type_ ")" "->" return_type {}
  | qualified_uident_ optional_type_arguments {}
  | "&" qualified_uident_ {}
  | "_" {}
  ;

return_type
  : type_ %prec prec_type {}
  | type_ "!" {}
  | type_ "!" separated_nonempty_list("+", error_type) {}
  ;

error_type
  : qualified_uident_ {}
  ;

optional_type_arguments
  : delimited("[", separated_nonempty_list(",", type_), "]") {}
  |  {}
  ;

parameter
  : type_ {}
  | POST_LABEL ":" type_ {}
  | POST_LABEL ":" type_ "=" ".." {}
  | POST_LABEL ":" type_ "=" "_" {}
  | label "?" ":" type_ {}
  ;

trait_method_parameter
  : type_ {}
  | POST_LABEL ":" type_ {}
  ;

constant
  : TRUE {}
  | FALSE {}
  | BYTE {}
  | BYTES {}
  | CHAR {}
  | INT {}
  | FLOAT {}
  | STRING {}
  ;

%inline vis
  :  {}
  | "priv" {}
  | "pub" pub_attr {}
  ;

pub_attr
  :  {}
  | "(" "readonly" ")" {}
  | "(" LIDENT ")" {}
  ;

%inline is_async
  : "async" {}
  |  {}
  ;

qualified_uident
  : UIDENT {}
  | PACKAGE_NAME DOT_UIDENT {}
  ;

qualified_uident_
  : UIDENT {}
  | PACKAGE_NAME DOT_UIDENT {}
  ;

uident
  : UIDENT {}
  ;

lident
  : LIDENT {}
  ;

label
  : LIDENT {}
  ;


