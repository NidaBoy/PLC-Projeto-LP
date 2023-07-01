(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* Distinguish between a identifier and a keyword of the language. *)
fun keyword (s, lpos, rpos) = 
	case s of 
	  "Bool"  => BOOL (lpos, rpos)
    | "else"  => ELSE (lpos, rpos)
    | "end"   => END (lpos, rpos)
    | "false" => CBOOL (false, lpos, rpos)
    | "fn"    => FN (lpos, rpos)
    | "fun"   => FUN (lpos, rpos)
    | "hd"    => HEAD (lpos, rpos)
    | "if"    => IF (lpos, rpos)
    | "match" => MATCH (lpos, rpos)
    | "Int"   => INT (lpos, rpos)
    | "ise"   => ISE (lpos, rpos)
    | "print" => PRINT (lpos, rpos)
    | "rec"   => REC (lpos, rpos)
    | "then"  => THEN (lpos, rpos)
    | "tl"    => TAIL (lpos, rpos)
    | "true"  => CBOOL (true, lpos, rpos)
    | "Nil"   => NIL (lpos, rpos)
    | "var"   => VAR (lpos, rpos)
    | "_"     => UNDERSCORE (lpos, rpos)
    | "with"  => WITH (lpos, rpos)
    | _       => NAME (s,lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")

val lineNumber = ref 0
val commentNestingLevel = ref 0

(* Get the current line being read. *)
fun getLineAsString() = 
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. This function also checks if we are within a comment, raising the "Unfinished Commentary" error. *)
fun eof () =
	let
		val CNL = !commentNestingLevel
	in
		if CNL = 0 then
			Tokens.EOF(0,0)
		else
			(TextIO.output(TextIO.stdOut, "\n*** Unfinished Commentary! ***\n");raise Fail("Unfinished commentary."))
	end

(* Convert a string into a integer value, if it is possible. *)
fun strToInt s =
	case Int.fromString s of
		  SOME i => i
	  | NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
%s COMMENT;
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%
<INITIAL,COMMENT>\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (CINT(strToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyword(yytext, yypos, yypos));
<INITIAL>"+" => (PLUS(yypos, yypos));
<INITIAL>"-" => (MINUS(yypos, yypos));
<INITIAL>"*" => (TIMES(yypos, yypos));
<INITIAL>"/" => (DIV(yypos, yypos));
<INITIAL>"(" => (LPAR(yypos, yypos));
<INITIAL>")" => (RPAR(yypos, yypos));
<INITIAL>"{" => (LBRACE(yypos, yypos));
<INITIAL>"}" => (RBRACE(yypos, yypos));
<INITIAL>"&&" => (AND(yypos, yypos));
<INITIAL>"!" => (NOT(yypos, yypos));
<INITIAL>"->" => (ARROW(yypos, yypos));
<INITIAL>"=>" => (DARROW(yypos, yypos));
<INITIAL>"=" => (EQ(yypos, yypos));
<INITIAL>"!=" => (NEQ(yypos, yypos));
<INITIAL>"<" => (LT(yypos, yypos));
<INITIAL>"<=" => (LTE(yypos, yypos));
<INITIAL>";" => (SEMIC(yypos, yypos));
<INITIAL>"[" => (LBRACK(yypos, yypos));
<INITIAL>"]" => (RBRACK(yypos, yypos));
<INITIAL>"," => (COMMA(yypos, yypos));
<INITIAL>"::" => (CONS(yypos, yypos));
<INITIAL>":" => (COLON(yypos, yypos));
<INITIAL>"|" => (PIPE(yypos, yypos));
<INITIAL>"(*" => (commentNestingLevel := !commentNestingLevel + 1; YYBEGIN COMMENT; lex());
<INITIAL>. => (error("\n***Lexer error: bad character ***\n"); raise Fail("Lexer error: bad character "^yytext));
<COMMENT>"(*" => (commentNestingLevel := !commentNestingLevel + 1; lex());
<COMMENT>"*)" => (commentNestingLevel := !commentNestingLevel - 1; if (!commentNestingLevel) = 0 then (YYBEGIN INITIAL; lex()) else lex());
<COMMENT>. => (lex());

