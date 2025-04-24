%start<unit> structure
%start<unit> expression

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
%token SEMI
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

%right BARBAR
%right AMPERAMPER
%left BAR
%left CARET
%left AMPER
%nonassoc prec_field
%nonassoc LPAREN
%left INFIX1
%left INFIX2
%left PLUS MINUS
%left INFIX3
%left INFIX4
%nonassoc prec_type
%nonassoc prec_apply_non_ident_fn
%nonassoc "!"
%nonassoc "?"

%%

non_empty_list_rev(X)
  : X {}
  | non_empty_list_rev(X) X {}
  ;

non_empty_list(X)
  : non_empty_list_rev(X) {}
  ;

non_empty_list_commas_rev(X)
  : X {}
  | non_empty_list_commas_rev(X) "," X {}
  ;

non_empty_list_commas_no_trailing(X)
  : non_empty_list_commas_rev(X) {}
  ;

non_empty_list_commas(X)
  : non_empty_list_commas_rev(X) option(",") {}
  ;

non_empty_list_commas_with_tail(X)
  : non_empty_list_commas_rev(X) "," {}
  ;

list_commas(X)
  :  {}
  | non_empty_list_commas(X) {}
  ;

list_commas_no_trailing(X)
  :  {}
  | non_empty_list_commas_no_trailing(X) {}
  ;

non_empty_list_commas_with_trailing_info(X)
  : non_empty_list_commas_rev(X) option(",") {}
  ;

list_commas_with_trailing_info(X)
  :  {}
  | non_empty_list_commas_with_trailing_info(X) {}
  ;

non_empty_list_semi_rev_aux(X)
  : X {}
  | non_empty_list_semi_rev_aux(X) SEMI X {}
  ;

non_empty_list_semis_rev(X)
  : non_empty_list_semi_rev_aux(X) option(SEMI) {}
  ;

none_empty_list_semis_rev_with_trailing_info(X)
  : non_empty_list_semi_rev_aux(X) option(SEMI) {}
  ;

non_empty_list_semis(X)
  : non_empty_list_semis_rev(X) {}
  ;

list_semis_rev(X)
  :  {}
  | non_empty_list_semis_rev(X) {}
  ;

list_semis(X)
  :  {}
  | non_empty_list_semis(X) {}
  ;

%inline id(x)
  : x {}
  ;

%inline annot
  : ":" type_ {}
  ;

%inline opt_annot
  : ioption(annot) {}
  ;

parameter
  : "_" opt_annot {}
  | binder opt_annot {}
  | POST_LABEL opt_annot {}
  | POST_LABEL opt_annot "=" expr {}
  | LIDENT "?" opt_annot {}
  ;

parameters
  : delimited("(", list_commas(parameter), ")") {}
  ;

type_parameters
  : delimited("[", non_empty_list_commas(id(tvar_binder)), "]") {}
  ;

%inline is_async
  : "async" {}
  |  {}
  ;

optional_type_parameters
  : option(type_parameters) {}
  ;

optional_type_parameters_no_constraints
  : option(delimited("[", non_empty_list_commas(id(type_decl_binder)), "]")) {}
  ;

optional_type_arguments
  : option(delimited("[", non_empty_list_commas(type_), "]")) {}
  ;

fun_binder
  : type_name "::" LIDENT {}
  | binder {}
  ;

fun_header
  : attributes visibility is_async "fn" fun_binder optional_bang optional_type_parameters option(parameters) option(preceded("->", return_type)) {}
  ;

local_type_decl
  : "struct" luident "{" list_semis(record_decl_field) "}" deriving_directive_list {}
  | "enum" luident "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | "type" luident type_ deriving_directive_list {}
  ;

extern_fun_header
  : attributes visibility "extern" STRING "fn" fun_binder optional_bang optional_type_parameters option(parameters) option(preceded("->", return_type)) {}
  ;

block_expr
  : "{" list_semis_rev(statement) "}" {}
  ;

local_types_and_stmts
  : local_type_decl {}
  | list_semis_rev(statement) {}
  | local_type_decl SEMI local_types_and_stmts {}
  ;

block_expr_with_local_types
  : "{" local_types_and_stmts "}" {}
  ;

impl_body
  : block_expr_with_local_types {}
  | "=" STRING {}
  ;

expression
  : expr EOF {}
  ;

val_header
  : attributes visibility "let" binder opt_annot {}
  | attributes visibility "const" UIDENT opt_annot {}
  ;

structure
  : list_semis(structure_item) EOF {}
  ;

structure_item
  : type_header deriving_directive_list {}
  | attributes visibility "extern" "type" luident optional_type_parameters_no_constraints deriving_directive_list {}
  | type_header type_ deriving_directive_list {}
  | type_header_bang option(type_) deriving_directive_list {}
  | type_header_bang "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | type_alias_header "=" type_ deriving_directive_list {}
  | struct_header "{" list_semis(record_decl_field) "}" deriving_directive_list {}
  | enum_header "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | val_header "=" expr {}
  | fun_header "=" STRING STRING {}
  | fun_header "=" STRING {}
  | fun_header "=" non_empty_list(MULTILINE_STRING) {}
  | extern_fun_header "=" STRING {}
  | extern_fun_header "=" non_empty_list(MULTILINE_STRING) {}
  | fun_header block_expr_with_local_types {}
  | attributes visibility "fnalias" func_alias_targets {}
  | attributes visibility "trait" luident option(preceded(COLON, separated_nonempty_list(PLUS, tvar_constraint))) "{" list_semis(trait_method_decl) "}" {}
  | attributes visibility "traitalias" luident "=" type_name {}
  | attributes visibility "typealias" PACKAGE_NAME batch_type_alias_targets {}
  | attributes visibility "traitalias" PACKAGE_NAME batch_trait_alias_targets {}
  | attributes "test" option(loced_string) option(parameters) block_expr_with_local_types {}
  | attributes visibility "impl" optional_type_parameters type_name "for" type_ "with" binder optional_bang parameters option(preceded("->", return_type)) impl_body {}
  | attributes visibility "impl" optional_type_parameters type_name "with" binder optional_bang parameters option(preceded("->", return_type)) impl_body {}
  ;

%inline attributes
  :  {}
  | non_empty_list(attribute) {}
  ;

%inline attribute
  : ATTRIBUTE {}
  ;

%inline visibility
  :  {}
  | "priv" {}
  | "pub" pub_attr {}
  ;

pub_attr
  :  {}
  | "(" "readonly" ")" {}
  | "(" LIDENT ")" {}
  ;

type_header
  : attributes visibility "type" luident optional_type_parameters_no_constraints {}
  ;

type_header_bang
  : attributes visibility "type" "!" luident {}
  ;

type_alias_header
  : attributes visibility "typealias" luident optional_type_parameters_no_constraints {}
  ;

struct_header
  : attributes visibility "struct" luident optional_type_parameters_no_constraints {}
  ;

enum_header
  : attributes visibility "enum" luident optional_type_parameters_no_constraints {}
  ;

batch_type_alias_targets
  : DOT_LIDENT optional_type_parameters_no_constraints {}
  | DOT_UIDENT optional_type_parameters_no_constraints {}
  | ".(" non_empty_list_commas(batch_type_alias_target) ")" {}
  ;

batch_type_alias_target
  : luident optional_type_parameters_no_constraints {}
  ;

batch_trait_alias_targets
  : DOT_LIDENT {}
  | DOT_UIDENT {}
  | ".(" non_empty_list_commas(batch_trait_alias_target) ")" {}
  ;

batch_trait_alias_target
  : luident {}
  ;

func_alias_targets
  : ioption(func_alias_type_name(LIDENT, UIDENT)) func_alias_target(LIDENT) {}
  | PACKAGE_NAME func_alias_target(DOT_LIDENT) {}
  | PACKAGE_NAME func_alias_type_name(DOT_LIDENT, DOT_UIDENT) func_alias_target(LIDENT) {}
  | option(func_alias_type_name(LIDENT, UIDENT)) "(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  | PACKAGE_NAME ".(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  | PACKAGE_NAME func_alias_type_name(DOT_LIDENT, DOT_UIDENT) "(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  ;

func_alias_type_name(LIDENT_MAYBE_DOT, UIDENT_MAYBE_DOT)
  : LIDENT_MAYBE_DOT "::" {}
 | UIDENT_MAYBE_DOT "::" {}
  ;

func_alias_target(LIDENT_MAYBE_DOT)
  : LIDENT_MAYBE_DOT option(preceded("as", binder)) {}
  ;

deriving_directive
  : type_name {}
  | type_name "(" list_commas(argument) ")" {}
  ;

deriving_directive_list
  :  {}
  | "derive" "(" list_commas(deriving_directive) ")" {}
  ;

trait_method_decl
  : binder optional_bang optional_type_parameters "(" list_commas(trait_method_param) ")" option(preceded("->", return_type)) option(preceded("=", wildcard)) {}
  ;

wildcard
  : "_" {}
  ;

trait_method_param
  : type_ {}
  | binder ":" type_ {}
  | POST_LABEL ":" type_ {}
  ;

luident
  : LIDENT {}
 | UIDENT {}
  ;

qual_ident
  : LIDENT {}
  | PACKAGE_NAME DOT_LIDENT {}
  ;

qual_ident_simple_expr
  : LIDENT %prec prec_apply_non_ident_fn {}
  | PACKAGE_NAME DOT_LIDENT {}
  ;

qual_ident_ty
  : luident {}
  | PACKAGE_NAME DOT_LIDENT {}
 | PACKAGE_NAME DOT_UIDENT {}
  ;

%inline semi_expr_semi_opt
  : none_empty_list_semis_rev_with_trailing_info(statement) {}
  ;

optional_bang
  : "!" {}
  |  {}
  ;

fn_header
  : "fn" binder optional_bang "{" {}
  ;

fn_header_no_binder
  : "fn" optional_bang "{" {}
  ;

statement
  : "let" pattern opt_annot "=" expr {}
  | "let" "mut" binder opt_annot "=" expr {}
  | is_async "fn" binder optional_bang parameters option(preceded("->", return_type)) block_expr {}
  | is_async fn_header list_semis(multi_pattern_case) "}" {}
  | guard_statement {}
  | expr_statement {}
  ;

guard_statement
  : "guard" infix_expr {}
  | "guard" infix_expr "else" block_expr {}
  | "guard" "let" pattern "=" infix_expr {}
  | "guard" "let" pattern "=" infix_expr "else" "{" single_pattern_cases "}" {}
  ;

%inline assignment_expr
  : left_value "=" expr {}
  ;

%inline augmented_assignment_expr
  : left_value assignop expr {}
  ;

expr_statement
  : "break" POST_LABEL option(expr) {}
  | "break" option(expr) {}
  | "continue" POST_LABEL list_commas_no_trailing(expr) {}
  | "continue" list_commas_no_trailing(expr) {}
  | "return" option(expr) {}
  | "raise" expr {}
  | "..." {}
  | augmented_assignment_expr {}
 | assignment_expr {}
 | expr {}
  ;

loop_label_colon
  : POST_LABEL ":" {}
  |  {}
  ;

while_expr
  : loop_label_colon "while" infix_expr block_expr optional_else {}
  ;

single_pattern_case
  : pattern option(preceded("if", expr)) "=>" expr_statement {}
  ;

single_pattern_cases
  : list_semis(single_pattern_case) {}
  ;

multi_pattern_case
  : non_empty_list_commas(pattern) option(preceded("if", expr)) "=>" expr_statement {}
  ;

catch_keyword
  : "catch" "{" {}
 | "{" {}
  | "catch" "!" "{" {}
  ;

%inline else_keyword
  : "else" "{" {}
  ;

try_expr
  : "try" expr catch_keyword single_pattern_cases "}" {}
  | "try" expr catch_keyword single_pattern_cases "}" else_keyword single_pattern_cases "}" {}
  ;

if_expr
  : "if" infix_expr block_expr "else" block_expr {}
 | "if" infix_expr block_expr "else" if_expr {}
  | "if" infix_expr block_expr {}
  ;

%inline match_header
  : "match" infix_expr "{" {}
  ;

match_expr
  : match_header non_empty_list_semis(single_pattern_case) "}" {}
  | match_header "}" {}
  ;

%inline loop_header
  : "loop" non_empty_list_commas_no_trailing(expr) "{" {}
  ;

loop_expr
  : loop_label_colon loop_header list_semis(multi_pattern_case) "}" {}
  ;

for_binders
  : list_commas_no_trailing(separated_pair(binder, "=", expr)) {}
  ;

optional_else
  : "else" block_expr {}
  |  {}
  ;

for_expr
  : loop_label_colon "for" for_binders SEMI option(infix_expr) SEMI list_commas_no_trailing(separated_pair(binder, "=", expr)) block_expr optional_else {}
  | loop_label_colon "for" for_binders block_expr optional_else {}
  ;

foreach_expr
  : loop_label_colon "for" non_empty_list_commas(foreach_binder) "in" expr block_expr optional_else {}
  ;

foreach_binder
  : binder {}
  | "_" {}
  ;

expr
  : loop_expr {}
 | for_expr {}
 | foreach_expr {}
 | while_expr {}
 | try_expr {}
 | if_expr {}
 | match_expr {}
 | pipe_expr {}
  ;

pipe_expr
  : pipe_expr "|>" infix_expr {}
  | infix_expr {}
  ;

infix_expr
  : infix_expr infixop infix_expr {}
  | postfix_expr {}
  ;

postfix_expr
  : range_expr "as" type_name {}
  | range_expr "is" range_pattern {}
  | range_expr {}
  ;

range_expr
  : prefix_expr "..<" prefix_expr {}
  | prefix_expr "..=" prefix_expr {}
  | prefix_expr {}
  ;

prefix_expr
  : id(plus) prefix_expr {}
  | id(minus) prefix_expr {}
  | simple_expr {}
  ;

%inline plus
  : PLUS {}
  ;

%inline minus
  : MINUS {}
  ;

left_value
  : var {}
  | simple_expr accessor {}
  | simple_expr "[" expr "]" {}
  ;

constr
  : UIDENT {}
  | PACKAGE_NAME DOT_UIDENT {}
  | type_name "::" UIDENT {}
  ;

%inline apply_attr
  :  {}
  | "!" {}
  | "!" "!" {}
  | "?" {}
  ;

simple_expr
  : "{" record_defn "}" {}
  | type_name COLONCOLON "{" list_commas_with_trailing_info(record_defn_single) "}" {}
  | ioption(terminated(type_name, COLONCOLON)) "{" ".." expr "}" {}
  | ioption(terminated(type_name, COLONCOLON)) "{" ".." expr "," list_commas(record_defn_single) "}" {}
  | "{" semi_expr_semi_opt "}" {}
  | "{" list_commas(map_expr_elem) "}" {}
  | is_async "fn" optional_bang parameters option(preceded("->", return_type)) block_expr {}
  | is_async fn_header_no_binder list_semis(multi_pattern_case) "}" {}
  | atomic_expr {}
  | "_" {}
  | qual_ident_simple_expr {}
  | constr {}
  | LIDENT "?" "(" list_commas(argument) ")" {}
  | simple_expr apply_attr "(" list_commas(argument) ")" {}
  | simple_expr "[" expr "]" {}
  | simple_expr "[" option(expr) ":" option(expr) "]" {}
  | simple_expr DOT_LIDENT apply_attr "(" list_commas(argument) ")" {}
  | simple_expr ".." LIDENT apply_attr "(" list_commas(argument) ")" {}
  | simple_expr accessor %prec prec_field {}
  | type_name "::" LIDENT {}
  | "(" list_commas(expr) ")" {}
  | "(" expr annot ")" {}
  | "[" list_commas(spreadable_elem) "]" {}
  ;

%inline label
  : LIDENT {}
  ;

%inline accessor
  : DOT_LIDENT {}
  | DOT_INT {}
  ;

%inline binder
  : LIDENT {}
  ;

tvar_binder
  : luident {}
  | luident COLON separated_nonempty_list(PLUS, tvar_constraint) {}
  ;

type_decl_binder
  : luident {}
  | "_" {}
  ;

tvar_constraint
  : qual_ident_ty {}
  | UIDENT "?" {}
  ;

%inline var
  : qual_ident {}
  ;

type_name
  : qual_ident_ty {}
  | "&" qual_ident_ty {}
  ;

multiline_string
  : MULTILINE_STRING {}
  | MULTILINE_INTERP {}
  ;

atomic_expr
  : simple_constant {}
  | non_empty_list(multiline_string) {}
  | INTERP {}
  ;

simple_constant
  : TRUE {}
  | FALSE {}
  | BYTE {}
  | BYTES {}
  | CHAR {}
  | INT {}
  | FLOAT {}
  | STRING {}
  ;

map_syntax_key
  : simple_constant {}
  | MINUS INT {}
  | MINUS FLOAT {}
  ;

%inline loced_string
  : STRING {}
  ;

%inline assignop
  : AUGMENTED_ASSIGNMENT {}
  ;

%inline infixop
  : INFIX4 {}
 | INFIX3 {}
 | INFIX2 {}
 | INFIX1 {}
  | PLUS {}
  | MINUS {}
  | AMPER {}
  | CARET {}
  | BAR {}
  | AMPERAMPER {}
  | BARBAR {}
  ;

optional_question
  : "?" {}
  |  {}
  ;

argument
  : label optional_question "=" expr {}
  | expr {}
  | POST_LABEL {}
  | LIDENT "?" {}
  ;

spreadable_elem
  : expr {}
  | ".." expr {}
  ;

map_expr_elem
  : map_syntax_key ":" expr {}
  ;

pattern
  : pattern "as" binder {}
  | or_pattern {}
  ;

or_pattern
  : range_pattern "|" or_pattern {}
  | range_pattern {}
  ;

range_pattern
  : simple_pattern "..<" simple_pattern {}
  | simple_pattern "..=" simple_pattern {}
  | simple_pattern {}
  ;

simple_pattern
  : TRUE {}
  | FALSE {}
  | CHAR {}
  | INT {}
  | BYTE {}
  | FLOAT {}
  | "-" INT {}
  | "-" FLOAT {}
  | STRING {}
  | BYTES {}
  | UNDERSCORE {}
  | binder {}
  | constr option(delimited("(", constr_pat_arguments, ")")) {}
  | "(" pattern ")" {}
  | "(" pattern "," non_empty_list_commas(pattern) ")" {}
  | "(" pattern annot ")" {}
  | "[" array_sub_patterns "]" {}
  | "{" "}" {}
  | "{" ".." option(",") "}" {}
  | "{" non_empty_fields_pat "}" {}
  | "{" non_empty_map_elems_pat "}" {}
  ;

array_sub_pattern
  : pattern {}
  | ".." STRING {}
  | ".." BYTES {}
  | ".." UIDENT {}
  | ".." PACKAGE_NAME DOT_UIDENT {}
  ;

dotdot_binder
  : ".." binder {}
  | ".." "_" {}
  | ".." "as" binder {}
  | ".." {}
  ;

array_sub_patterns
  :  {}
  | array_sub_pattern {}
  | array_sub_pattern "," array_sub_patterns {}
  | dotdot_binder "," non_empty_list_commas(array_sub_pattern) {}
  | dotdot_binder ioption(",") {}
  ;

return_type
  : type_ %prec prec_type {}
  | type_ "!" {}
  | type_ "!" separated_nonempty_list("+", error_type) {}
  ;

error_type
  : qual_ident_ty {}
  | "_" {}
  ;

type_
  : type_ "?" {}
  | "(" type_ "," non_empty_list_commas(type_) ")" {}
  | is_async "(" type_ "," ioption(non_empty_list_commas(type_)) ")" "->" return_type {}
  | is_async "(" ")" "->" return_type {}
  | "(" type_ ")" {}
  | is_async "(" type_ ")" "->" return_type {}
  | qual_ident_ty optional_type_arguments {}
  | "&" qual_ident_ty {}
  | "_" {}
  ;

record_decl_field
  : visibility option("mut") LIDENT ":" type_ {}
  ;

constructor_param
  : option("mut") type_ {}
  | option("mut") POST_LABEL ":" type_ {}
  ;

enum_constructor
  : UIDENT option(delimited("(", non_empty_list_commas(constructor_param), ")")) option(eq_int_tag) {}
  ;

%inline eq_int_tag
  : "=" INT {}
  ;

record_defn
  : label_pun "," list_commas_with_trailing_info(record_defn_single) {}
  | labeled_expr option(",") {}
  | labeled_expr "," non_empty_list_commas_with_trailing_info(record_defn_single) {}
  ;

record_defn_single
  : labeled_expr {}
 | label_pun {}
  ;

%inline labeled_expr
  : label ":" expr {}
  ;

%inline label_pun
  : label {}
  ;

non_empty_fields_pat
  : non_empty_list_commas(fields_pat_single) {}
  | non_empty_list_commas_with_tail(fields_pat_single) ".." option(",") {}
  ;

fields_pat_single
  : fpat_labeled_pattern {}
 | fpat_label_pun {}
  ;

%inline fpat_labeled_pattern
  : label ":" pattern {}
  ;

%inline fpat_label_pun
  : label {}
  ;

non_empty_map_elems_pat
  : non_empty_list_commas(map_elem_pat) {}
  | non_empty_list_commas_with_tail(map_elem_pat) ".." option(",") {}
  ;

%inline map_elem_pat
  : map_syntax_key option("?") ":" pattern {}
  ;

constr_pat_arguments
  : constr_pat_argument option(",") {}
  | ".." option(",") {}
  | constr_pat_argument "," constr_pat_arguments {}
  ;

constr_pat_argument
  : label "=" pattern {}
  | POST_LABEL {}
  | pattern {}
  ;


