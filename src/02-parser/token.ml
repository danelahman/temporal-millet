type token =
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COLON
  | COMMA
  | SEMI
  | EQUAL
  | CONS
  | BEGIN
  | END
  | LNAME of string
  | UNDERSCORE
  | AS
  | INT of int
  | STRING of string
  | BOOL of bool
  | FLOAT of float
  | UNAME of SugaredAst.label
  | PARAM of SugaredAst.ty_param
  | TYPE
  | OPERATION
  | ARROW
  | SIGARROW
  | OF
  | HASH
  | MATCH
  | WITH
  | FUNCTION
  | HANDLER
  | HANDLE
  | CONTINUE
  | RUN
  | LET
  | REC
  | AND
  | IN
  | DELAY
  | BOX
  | UNBOX
  | PERFORM
  | FUN
  | BAR
  | BARBAR
  | IF
  | THEN
  | ELSE
  | PLUS
  | STAR
  | MINUS
  | MINUSDOT
  | LSL
  | LSR
  | ASR
  | MOD
  | OR
  | AMPER
  | AMPERAMPER
  | LAND
  | LOR
  | LXOR
  | PREFIXOP of string
  | INFIXOP0 of string
  | INFIXOP1 of string
  | INFIXOP2 of string
  | INFIXOP3 of string
  | INFIXOP4 of string
  | RESOURCES
  | EOF
