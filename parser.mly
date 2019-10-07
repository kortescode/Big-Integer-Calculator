%token <Bigint.Bigint.bigint> BIGINT
%token ADD SUB MUL DIV MODULO PARO PARC EOL QUIT
%left ADD SUB
%left MUL DIV MODULO
%start main
%type <Arithexpr.ArithExpr.arith_expr> main
%%

main: expr EOL		{ $1 };

expr:
  | BIGINT		{ Arithexpr.ArithExpr.value($1) }
  | PARO expr PARC	{ $2 }
  | expr ADD expr	{ Arithexpr.ArithExpr.add($1, $3) }
  | expr SUB expr	{ Arithexpr.ArithExpr.sub($1, $3) }
  | expr MUL expr	{ Arithexpr.ArithExpr.mul($1, $3) }
  | expr DIV expr	{ Arithexpr.ArithExpr.div($1, $3) }
  | expr MODULO expr	{ Arithexpr.ArithExpr.modulo($1, $3) }
  | QUIT		{ exit 0 };
