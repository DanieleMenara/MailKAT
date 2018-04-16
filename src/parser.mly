%{
open Action
open Test
%}

%token <string> ID
%token <string> STRING
%token ONE
%token ZERO
%token NULL
%token LEFT_SQUARE
%token RIGHT_SQUARE
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEQ
%token PLUS
%token EQUAL
%token STAR
%token STORE
%token RED
%token ASSIGN
%token NEG
%token EOF

%left PLUS              /* lowest precedence */
%left SEQ               /* medium precedence */
%nonassoc NEG           /* highest precedence */
%nonassoc STAR          /* highest precedence */

%start <Action.fmla option> prog
%type <Action.fmla> action
%type <Test.tests> tests
%type <Test.tests> test
%%

prog:
  | s = action EOF                                         { Some s }
  | EOF                                                    { None } ;

action:
  | t = test                                                { t :> fmla }
  | field = ID; ASSIGN; value = ID                          { `Mod(field, value) }
  | RED; LEFT_SQUARE; add = STRING; RIGHT_SQUARE            { `Red(add) }
  | STORE; LEFT_SQUARE; mailbox = STRING; RIGHT_SQUARE      { `Store(mailbox) }
  | a = delimited(LEFT_BRACK, action, RIGHT_BRACK)          { a }
  | a = separated_pair(action, PLUS, action)                { `Sum(a) }
  | a = separated_pair(action, SEQ, action)                 { `Seq(a) }
  | a = action; STAR                                        { `Star(a) } ;

tests:
  | t = test                                                { t }
  | t = separated_pair(tests, PLUS, tests)                  { `Sum(t) }
  | t = separated_pair(tests, SEQ, tests)                   { `Seq(t) }
  | t = delimited(LEFT_BRACK, tests, RIGHT_BRACK)           { t } ;

test:
  | ZERO                                                    { `Const(false) }
  | ONE                                                     { `Const(true) }
  | NEG; t = tests                                          { `Not(t) }
  | field = ID; EQUAL; value = STRING                       { `Test(field, (Some value)) }
  | field = ID; EQUAL; value = NULL                         { `Test(field, None) } ;
