%%

%name PlcParser

%pos int

%term VAR | FUN | REC | DARROW | FN | END
		| IF | THEN | ELSE
		| MATCH | WITH | PIPE | UNDERSCORE
		| NOT | AND
		| HEAD | TAIL | CONS | ISE
		| PRINT
		| PLUS | MINUS | TIMES | DIV 
		| EQ | NEQ | LT | LTE
		| LPAR | RPAR | LBRACE | RBRACE | LBRACK | RBRACK
		| COMMA | COLON | SEMIC
		| NIL | BOOL | INT | ARROW | NAME of string | CINT of int | CBOOL of bool
		| EOF

%nonterm Prog of expr | Decl | Expr of expr | AtomExpr of expr |  AppExpr of expr | Const of expr | Comps of expr list | MatchExpr of (expr option * expr) list | CondExpr of expr option | Args of (plcType * string) list | Params of (plcType * string) list | TypedVar of plcType * string | Type of plcType | AtomType of plcType | Types of plcType list | RetType of plcType

%right SEMIC ARROW
%nonassoc IF MATCH
%left ELSE
%left AND
%left EQ NEQ
%left LT LTE
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HEAD TAIL ISE PRINT NAME
%left LBRACK

%eop EOF

%noshift EOF

%value CINT(0)

%start Prog

%%


Prog: Expr (Expr)
	| VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
	| FUN NAME Args EQ Expr SEMIC Prog (let 
		val e = makeAnon (Args, Expr)
	in
		Let(NAME, e, Prog)
	end)
	| FUN REC NAME Args RetType EQ Expr SEMIC Prog (makeFun(NAME, Args, RetType, Expr, Prog))


RetType: COLON Type (Type)


Args: LPAR RPAR ([])
	| LPAR Params RPAR (Params)


Params: TypedVar (TypedVar :: [])
	| TypedVar COMMA Params (TypedVar :: Params)


TypedVar : Type NAME ((Type, NAME))


Type: AtomType (AtomType)
	| LPAR Types RPAR (ListT Types)
	| LBRACK Type RBRACK (SeqT Type)
	| Type ARROW Type (FunT (Type1,Type2))


AtomType: INT (IntT)
	| BOOL (BoolT)
	| NIL ( ListT [])
	| LPAR Type RPAR (Type)


Types: Type COMMA Type ([Type1,Type2])
	| Type COMMA Types (Type::Types)


Expr : AtomExpr (AtomExpr)
	| AppExpr (AppExpr)
	| IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
	| MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
	| MINUS Expr (Prim1("-", Expr1))
	| NOT Expr (Prim1("!", Expr1))
	| HEAD Expr (Prim1("hd", Expr1))
	| TAIL Expr (Prim1("tl", Expr1))
	| ISE Expr (Prim1("ise", Expr1))
	| PRINT Expr (Prim1("print", Expr1))
	| Expr AND Expr (Prim2("&&", Expr1, Expr2))
	| Expr PLUS Expr (Prim2("+", Expr1, Expr2))
	| Expr MINUS Expr (Prim2("-", Expr1, Expr2))
	| Expr TIMES Expr (Prim2("*", Expr1, Expr2))
	| Expr DIV Expr (Prim2("/", Expr1, Expr2))
	| Expr EQ Expr (Prim2("=", Expr1, Expr2))
	| Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
	| Expr LT Expr (Prim2("<", Expr1, Expr2))
	| Expr LTE Expr (Prim2("<=", Expr1, Expr2))
	| Expr CONS Expr (Prim2("::", Expr1, Expr2))
	| Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
	| Expr LBRACK CINT RBRACK (Item(CINT, Expr1))


AtomExpr : Const (Const)
	| NAME (Var(NAME))
	| LBRACE Prog RBRACE (Prog1)
	| LPAR Expr RPAR (Expr1)
	| LPAR Comps RPAR (List(Comps1))
	| FN Args DARROW Expr END (makeAnon(Args, Expr))


AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
	| AppExpr AtomExpr (Call(AppExpr, AtomExpr))


MatchExpr : END ([])
	| PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)


CondExpr : UNDERSCORE (NONE)
	| Expr (SOME (Expr))


Comps: Expr COMMA Expr ([Expr1, Expr2])
	| Expr COMMA Comps (Expr1 :: Comps1)


Const : CINT (ConI(CINT))
	| CBOOL (ConB(CBOOL))
	| LPAR RPAR (List [])
	| LPAR Type LBRACK RBRACK RPAR (ESeq (Type))

