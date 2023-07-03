%skeleton "lalr1.cc"
%require "2.5"
%defines
%define api.namespace {translator}
%define api.value.type variant
%define parser_class_name {Parser}

%code requires {
    #include <iostream>
    #include <memory>
    #include "ast.h"
    namespace translator {class Lexer;}
}

%parse-param {translator::Lexer& lexer} {std::shared_ptr<Ast>& result} {std::string& message}

%code {
    #include "lexer.h"
    #define yylex lexer.lex
}

%token END 0 "end of file"
%token ERROR

%token <int> NUM

%token LPAR "("
%token RPAR ")"
%token LBR "["
%token RBR "]"
%token LTK "<"
%token RTK ">"
%token BUILTIN "builtin"
%token CON "con"
%token PROGRAM "program"
%token FORCE "force"
%token DELAY "delay"
%token LAM "lam"

%token INTEGER "integer"
%token BYTESTRING "bytestring"
%token LIST "list"
%token PAIR "pair"

%token <std::string> NAME
%token <std::string> VERSION

%type <std::shared_ptr<Ast>> term con

%nonassoc UMINUS

%%

input: "(" "program" VERSION term ")" { result = $4; }

con: "(" "con" "integer" NUM ")" { $$ = new_integer($4); }
;

term: con { $$ = $1; }
    | "[" term term "]" { $$ = new_apply($2, $3); }
    | "(" "builtin" NAME ")" { $$ = new_builtin($3); }
    | "(" "force" term ")" { $$ = new_force($3); }
    | "(" "delay" term ")" { $$ = new_delay($3); }
    | "(" "lam" NAME term ")" { $$ = new_lambda($3, $4); }
    | NAME { $$ = new_variable($1); }
;

%%

void translator::Parser::error(const std::string& err)
{
    message = err;
}
