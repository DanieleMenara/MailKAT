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

%start <Action.action option> prog
%type <Action.action> actions
%type <Action.action> action
%type <Test.test> test
%%

prog:
  | s = actions EOF                                         { Some s }
  | EOF                                                     { None   } ;

actions:
  | a = action                                              { a }
  | acts = delimited(LEFT_BRACK, actions, RIGHT_BRACK)      { acts }
  | a1 = actions; PLUS; a2 = actions                        { Sum(a1, a2) }
  | a1 = actions; SEQ; a2 = actions                         { Seq(a1, a2) } ;

test:
  | ZERO                                                    { Const(false) }
  | ONE                                                     { Const(true) }
  | NEG; t = test                                           { Neg(t) }
  | field = ID; EQUAL; value = ID                           { Test(field, value) } ;

action:
  | t = test                                                { Test(t) }
  | field = ID; ASSIGN; value = ID                          { Mod(field, value) }
  | RED; LEFT_SQUARE; id = ID; RIGHT_SQUARE                 { Red(id) }
  | STORE; LEFT_SQUARE; id = ID; RIGHT_SQUARE               { Store(id) } ;
