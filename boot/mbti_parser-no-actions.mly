%start<unit> t

%token CHAR
%token INT
%token BYTE
%token BYTES
%token FLOAT
%token DOUBLE
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
%token TRY_QUESTION "try?"
%token TRY_EXCLAMATION "try!"
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
%token USING "using"
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
%token DEFER "defer"
%token FOR "for"
%token IN "in"
%token IS "is"
%token SUBERROR "suberror"
%token AND "and"
%token LETREC "letrec"
%token ENUMVIEW "enumview"
%token NORAISE "noraise"

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

type_name_coloncolon
  : uident "::" {}
  ;

func_sig
  : attributes is_async FN option(type_name_coloncolon) lident loption(type_params_with_constraints) delimited("(", separated_list(",", parameter), ")") "->" return_type {}
  | attributes is_async FN type_params_with_constraints option(type_name_coloncolon) lident delimited("(", separated_list(",", parameter), ")") "->" return_type {}
  ;

trait_method_sig
  : attributes lident delimited("(", separated_list(",", trait_method_parameter), ")") "->" return_type option(eq_underscore) {}
  ;

%inline eq_underscore
  : "=" "_" {}
  ;

suberror_keyword
  : "suberror" {}
  | "type" "!" {}
  ;

type_sig
  : attributes vis "extern" "type" type_decl_name_with_params {}
  | attributes vis "type" type_decl_name_with_params {}
  | attributes vis suberror_keyword UIDENT option(type_) {}
  | attributes vis suberror_keyword UIDENT "{" separated_list(";", enum_constructor) "}" {}
  | attributes vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" {}
  | attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" {}
  | attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" {}
  ;

impl_sig
  : attributes "impl" type_params_with_constraints qualified_uident "for" type_ {}
  | attributes "impl" qualified_uident "for" type_ {}
  ;

trait_sig
  : vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" {}
  | vis "trait" uident {}
  ;

alias_sig
  : vis "typealias" type_ "as" type_decl_name_with_params {}
  | vis "traitalias" qualified_uident "as" uident {}
  | "fnalias" uident "::" lident {}
  ;

enum_constructor
  : attributes UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) {}
  ;

%inline eq_tag
  : "=" INT {}
  ;

constructor_param
  : option("mut") type_ {}
  | option("mut") POST_LABEL ":" type_ {}
  ;

record_decl_field
  : attributes option("mut") LIDENT ":" type_ {}
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

simple_type
  : simple_type "?" {}
  | "(" type_ "," separated_nonempty_list(",", type_) ")" {}
  | "(" type_ ")" {}
  | qualified_uident_ optional_type_arguments {}
  | "&" qualified_uident_ {}
  | "_" {}
  ;

type_
  : simple_type {}
  | is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type {}
  | is_async "(" ")" "->" return_type {}
  | is_async "(" type_ ")" "->" return_type {}
  ;

return_type
  : type_ {}
  | simple_type "!" {}
  | simple_type "!" error_type {}
  | simple_type "?" error_type {}
  | simple_type "raise" {}
  | simple_type "raise" error_type {}
  | simple_type "raise" "?" {}
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
  | DOUBLE {}
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

%inline attributes
  :  {}
  | nonempty_list(attribute) {}
  ;

%inline attribute
  : ATTRIBUTE {}
  ;


