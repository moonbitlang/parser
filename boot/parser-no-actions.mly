%start<unit> structure
%start<unit> expression

%token CHAR
%token INT
%token BYTE
%token BYTES
%token DOUBLE
%token FLOAT
%token STRING
%token MULTILINE_STRING
%token MULTILINE_INTERP
%token INTERP
%token REGEX_LITERAL
%token REGEX_INTERP
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
%token TRY_QUESTION "try?"
%token TRY_EXCLAMATION "try!"
%token LEXMATCH "lexmatch"

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
%nonassoc prec_lower_than_as
%nonassoc "as"
%nonassoc prec_apply_non_ident_fn
%nonassoc "!"
%nonassoc prec_lower_than_arrow_fn
%nonassoc ","
%nonassoc ")"
%nonassoc ":"

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
  | LIDENT "?" opt_annot "=" expr {}
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

%inline optional_type_arguments
  : ioption(delimited("[", non_empty_list_commas(type_), "]")) {}
  ;

fun_binder
  : type_name "::" LIDENT {}
  | binder {}
  ;

fun_header
  : attributes visibility is_async fun_header_generic option(parameters) func_return_type {}
  ;

fun_header_generic
  : "fn" type_parameters fun_binder optional_bang {}
  | "fn" fun_binder optional_bang optional_type_parameters {}
  ;

local_type_decl
  : "struct" UIDENT "{" list_semis(record_decl_field) "}" deriving_directive_list {}
  | "struct" UIDENT "(" non_empty_list_commas(type_) ")" deriving_directive_list {}
  | "enum" UIDENT "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | "type" UIDENT type_ deriving_directive_list {}
  ;

extern_fun_header
  : attributes visibility "extern" STRING "fn" fun_binder optional_bang optional_type_parameters option(parameters) func_return_type {}
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
  | attributes visibility "extern" "type" UIDENT optional_type_parameters_no_constraints deriving_directive_list {}
  | type_header type_ deriving_directive_list {}
  | suberror_header option(type_) deriving_directive_list {}
  | suberror_header "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | struct_header "{" list_semis(record_decl_field) "}" deriving_directive_list {}
  | struct_header "(" non_empty_list_commas(type_) ")" deriving_directive_list {}
  | enum_header "{" list_semis(enum_constructor) "}" deriving_directive_list {}
  | val_header "=" expr {}
  | fun_header "=" STRING STRING {}
  | fun_header "=" STRING {}
  | fun_header "=" non_empty_list(MULTILINE_STRING) {}
  | extern_fun_header "=" STRING {}
  | extern_fun_header "=" non_empty_list(MULTILINE_STRING) {}
  | fun_header block_expr_with_local_types {}
  | attributes visibility "fnalias" func_alias_targets {}
  | attributes visibility "trait" UIDENT option(preceded(COLON, separated_nonempty_list(PLUS, tvar_constraint))) "{" list_semis(trait_method_decl) "}" {}
  | attributes visibility "traitalias" UIDENT "=" type_name {}
  | attributes visibility "typealias" batch_type_alias_targets {}
  | attributes visibility "typealias" type_ "=" type_ deriving_directive_list {}
  | attributes visibility "typealias" type_ "as" UIDENT optional_type_parameters_no_constraints {}
  | attributes visibility "traitalias" batch_type_alias_targets {}
  | attributes "test" option(loced_string) option(parameters) block_expr_with_local_types {}
  | attributes visibility "impl" optional_type_parameters type_name "for" type_ "with" binder optional_bang parameters func_return_type impl_body {}
  | attributes visibility "impl" optional_type_parameters type_name "with" binder optional_bang parameters func_return_type impl_body {}
  | attributes visibility "impl" optional_type_parameters type_name "for" type_ {}
  | attributes visibility "enumview" optional_type_parameters UIDENT "{" list_semis(enum_constructor) "}" "for" type_ "with" binder parameters block_expr {}
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
  : attributes visibility "type" UIDENT optional_type_parameters_no_constraints {}
  ;

suberror_header
  : attributes visibility "type" "!" UIDENT {}
  | attributes visibility "suberror" UIDENT {}
  ;

struct_header
  : attributes visibility "struct" UIDENT optional_type_parameters_no_constraints {}
  ;

enum_header
  : attributes visibility "enum" UIDENT optional_type_parameters_no_constraints {}
  ;

batch_type_alias_targets
  : PACKAGE_NAME batch_type_alias_target(DOT_UIDENT) {}
  | PACKAGE_NAME ".(" non_empty_list_commas(batch_type_alias_target(UIDENT)) ")" {}
  | batch_type_alias_target(UIDENT) {}
  ;

batch_type_alias_target(UIDENT_MAYBE_DOT)
  : UIDENT_MAYBE_DOT {}
  | UIDENT_MAYBE_DOT "as" UIDENT {}
  ;

func_alias_targets
  : ioption(func_alias_type_name(UIDENT)) func_alias_target(LIDENT) {}
  | PACKAGE_NAME func_alias_target(DOT_LIDENT) {}
  | PACKAGE_NAME func_alias_type_name(DOT_UIDENT) func_alias_target(LIDENT) {}
  | option(func_alias_type_name(UIDENT)) "(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  | PACKAGE_NAME ".(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  | PACKAGE_NAME func_alias_type_name(DOT_UIDENT) "(" non_empty_list_commas(func_alias_target(LIDENT)) ")" {}
  ;

func_alias_type_name(UIDENT_MAYBE_DOT)
  : UIDENT_MAYBE_DOT "::" {}
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
  : attributes is_async binder optional_bang optional_type_parameters "(" list_commas(trait_method_param) ")" func_return_type option(preceded("=", wildcard)) {}
  ;

wildcard
  : "_" {}
  ;

trait_method_param
  : type_ {}
  | binder ":" type_ {}
  | POST_LABEL ":" type_ {}
  | LIDENT "?" ":" type_ {}
  ;

qual_ident
  : LIDENT {}
  | PACKAGE_NAME DOT_LIDENT {}
  ;

qual_ident_simple_expr
  : LIDENT %prec prec_apply_non_ident_fn {}
  | PACKAGE_NAME DOT_LIDENT {}
  ;

%inline qual_ident_ty_inline
  : UIDENT {}
  | PACKAGE_NAME DOT_UIDENT {}
  ;

qual_ident_ty
  : qual_ident_ty_inline {}
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

letand_func
  : arrow_fn_expr {}
  | anony_fn {}
  ;

and_func
  : "and" binder opt_annot "=" letand_func {}
  ;

statement
  : "let" pattern opt_annot "=" expr {}
  | "letrec" binder opt_annot "=" letand_func list(and_func) {}
  | "let" "mut" binder opt_annot "=" expr {}
  | is_async "fn" binder optional_bang parameters func_return_type block_expr {}
  | is_async fn_header list_semis(multi_pattern_case) "}" {}
  | guard_statement {}
  | "defer" pipe_expr {}
  | expr_statement {}
  ;

guard_statement
  : "guard" infix_expr {}
  | "guard" infix_expr "else" block_expr {}
  ;

%inline assignment_expr
  : left_value "=" expr {}
  ;

%inline augmented_assignment_expr
  : left_value assignop expr {}
  ;

expr_statement_no_break_continue_return
  : "raise" expr {}
  | "..." {}
  | augmented_assignment_expr {}
 | assignment_expr {}
 | expr {}
  ;

expr_statement
  : "break" POST_LABEL option(expr) {}
  | "break" option(expr) {}
  | "continue" POST_LABEL list_commas_no_trailing(expr) {}
  | "continue" list_commas_no_trailing(expr) {}
  | "return" option(expr) {}
  | expr_statement_no_break_continue_return {}
  ;

loop_label_colon
  : POST_LABEL ":" {}
  |  {}
  ;

while_expr
  : loop_label_colon "while" infix_expr block_expr optional_else {}
  ;

single_pattern_case
  : pattern option(preceded("if", infix_expr)) "=>" expr_statement {}
  | "..." {}
  ;

single_pattern_cases
  : list_semis(single_pattern_case) {}
  ;

multi_pattern_case
  : non_empty_list_commas(pattern) option(preceded("if", infix_expr)) "=>" expr_statement {}
  ;

catch_keyword
  : "catch" "{" {}
  | "catch" "!" "{" {}
  ;

%inline else_keyword
  : "else" "{" {}
  | "noraise" "{" {}
  ;

try_expr
  : "try" pipe_expr catch_keyword single_pattern_cases "}" {}
  | "try" pipe_expr catch_keyword single_pattern_cases "}" else_keyword single_pattern_cases "}" {}
  | "try?" pipe_expr {}
  | "try!" pipe_expr {}
  ;

if_expr
  : "if" infix_expr block_expr "else" block_expr {}
 | "if" infix_expr block_expr "else" if_expr {}
  | "if" infix_expr block_expr {}
  ;

match_header
  : "match" infix_expr "{" {}
  | "match" infix_expr "using" label "{" {}
  ;

match_expr
  : match_header non_empty_list_semis(single_pattern_case) "}" {}
  | match_header "}" {}
  ;

lex_expr
  : lex_header lex_cases "}" {}
  ;

lex_header
  : "lexmatch" infix_expr "{" {}
  | "lexmatch" infix_expr "using" label "{" {}
  ;

lex_cases
  :  {}
  | SEMI {}
  | lex_catchall_case {}
  | lex_catchall_case SEMI {}
  | lex_case SEMI lex_cases {}
  ;

lex_catchall_case
  : "_" "=>" expr_statement {}
  | binder "=>" expr_statement {}
  | "..." {}
  ;

lex_case
  : lex_sequence_pattern "=>" expr_statement {}
  | lex_sequence_pattern "," lext_case_rest_binder "=>" expr_statement {}
  ;

lext_case_rest_binder
  : "_" {}
  | binder {}
  ;

lex_sequence_pattern
  : lex_pattern_sequence {}
  | lex_atom_pattern "as" binder {}
  ;

lex_pattern_sequence
  : lex_atom_pattern {}
  | lex_atom_pattern option(SEMI) lex_pattern_sequence {}
  ;

lex_atom_pattern
  : REGEX_LITERAL {}
  | REGEX_INTERP {}
  | STRING {}
  | INTERP {}
  | "(" lex_sequence_pattern ")" {}
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
 | lex_expr {}
 | simple_try_expr {}
  | arrow_fn_expr {}
  ;

simple_try_expr
  : pipe_expr catch_keyword single_pattern_cases "}" {}
  | pipe_expr {}
  ;

arrow_fn_expr
  : "(" arrow_fn_prefix "=>" expr_statement_no_break_continue_return {}
  | "(" ")" "=>" expr_statement_no_break_continue_return {}
  | binder "=>" expr_statement_no_break_continue_return {}
  | "_" "=>" expr_statement_no_break_continue_return {}
  ;

arrow_fn_prefix
  : binder ioption(",") ")" {}
  | "_" ioption(",") ")" {}
  | binder ":" type_ ioption(",") ")" {}
  | "_" ":" type_ ioption(",") ")" {}
  | binder "," arrow_fn_prefix {}
  | "_" "," arrow_fn_prefix {}
  | binder ":" type_ "," arrow_fn_prefix {}
  | "_" ":" type_ "," arrow_fn_prefix {}
  ;

arrow_fn_prefix_no_constraint
  : binder ioption(",") ")" {}
  | "_" ioption(",") ")" {}
  | binder "," arrow_fn_prefix_no_constraint {}
  | "_" "," arrow_fn_prefix_no_constraint {}
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
  | "!" prefix_expr {}
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
  ;

non_empty_tuple_elems
  : expr ioption(",") ")" {}
  | expr "," non_empty_tuple_elems {}
  ;

non_empty_tuple_elems_with_prefix
  : binder "," non_empty_tuple_elems_with_prefix {}
  | "_" "," non_empty_tuple_elems_with_prefix {}
  | non_empty_tuple_elems {}
  ;

tuple_expr
  : "(" arrow_fn_prefix_no_constraint {}
  | "(" non_empty_tuple_elems_with_prefix {}
  | "(" binder ":" type_ ")" {}
  | "(" "_" ":" type_ ")" {}
  | "(" expr ":" type_ ")" {}
  | "(" ")" {}
  ;

anony_fn
  : is_async "fn" optional_bang parameters func_return_type block_expr {}
  | is_async fn_header_no_binder list_semis(multi_pattern_case) "}" {}
  ;

simple_expr
  : "{" record_defn "}" {}
  | type_name COLONCOLON "{" list_commas_with_trailing_info(record_defn_single) "}" {}
  | ioption(terminated(type_name, COLONCOLON)) "{" ".." expr "}" {}
  | ioption(terminated(type_name, COLONCOLON)) "{" ".." expr "," list_commas(record_defn_single) "}" {}
  | "{" semi_expr_semi_opt "}" {}
  | "{" list_commas(map_expr_elem) "}" {}
  | anony_fn {}
  | atomic_expr {}
  | "_" %prec prec_lower_than_arrow_fn {}
  | qual_ident_simple_expr {}
  | constr {}
  | simple_expr apply_attr "(" list_commas(argument) ")" {}
  | simple_expr "[" expr "]" {}
  | simple_expr "[" option(expr) ":" option(expr) "]" {}
  | simple_expr DOT_LIDENT apply_attr "(" list_commas(argument) ")" {}
  | simple_expr ".." LIDENT apply_attr "(" list_commas(argument) ")" {}
  | simple_expr accessor %prec prec_field {}
  | type_name "::" LIDENT {}
  | "[" list_commas(spreadable_elem) "]" {}
  | tuple_expr {}
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
  : UIDENT {}
  | UIDENT COLON separated_nonempty_list(PLUS, tvar_constraint) {}
  ;

type_decl_binder
  : UIDENT {}
  | "_" {}
  ;

tvar_constraint
  : qual_ident_ty {}
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
  | DOUBLE {}
  | FLOAT {}
  | STRING {}
  ;

map_syntax_key
  : simple_constant {}
  | MINUS INT {}
  | MINUS DOUBLE {}
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
  | POST_LABEL "=" expr {}
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
  | DOUBLE {}
  | FLOAT {}
  | "-" INT {}
  | "-" DOUBLE {}
  | "-" FLOAT {}
  | STRING {}
  | BYTES {}
  | REGEX_LITERAL {}
  | UNDERSCORE {}
  | binder {}
  | constr option(delimited("(", constr_pat_arguments, ")")) {}
  | binder delimited("(", constr_pat_arguments_no_open, ")") {}
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

error_annotation
  : "raise" {}
  | "raise" error_type {}
  | "noraise" {}
  | "raise" "?" {}
  ;

return_type
  : type_ {}
  | simple_type "!" {}
  | simple_type "!" error_type {}
  | simple_type "?" error_type {}
  | simple_type error_annotation {}
  ;

func_return_type
  : "->" return_type {}
  | error_annotation {}
  |  {}
  ;

error_type
  : qual_ident_ty {}
  | "_" {}
  ;

simple_type
  : simple_type "?" {}
  | "(" type_ "," non_empty_list_commas(type_) ")" {}
  | "(" type_ ")" {}
  | qual_ident_ty_inline optional_type_arguments %prec prec_lower_than_as {}
  | "&" qual_ident_ty {}
  | "_" {}
  ;

type_
  : simple_type {}
  | is_async "(" type_ "," ioption(non_empty_list_commas(type_)) ")" "->" return_type {}
  | is_async "(" ")" "->" return_type {}
  | is_async "(" type_ ")" "->" return_type {}
  ;

record_decl_field
  : attributes visibility option("mut") LIDENT ":" type_ {}
  ;

constructor_param
  : option("mut") type_ {}
  | option("mut") POST_LABEL ":" type_ {}
  ;

enum_constructor
  : attributes UIDENT option(delimited("(", non_empty_list_commas(constructor_param), ")")) option(eq_int_tag) {}
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

constr_pat_arguments_no_open
  : constr_pat_argument option(",") {}
  | constr_pat_argument "," constr_pat_arguments_no_open {}
  ;

constr_pat_argument
  : label "=" pattern {}
  | POST_LABEL "=" pattern {}
  | POST_LABEL {}
  | pattern {}
  ;


