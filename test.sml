(* Infrastructure to run the Plc interpreter *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

val abs = fromFile "tests/t3.plc";
val venv = [];

(* testa somente os valores *)
(* val v = eval abs venv;  *)

(* testa somente os tipos *)
val tv = teval abs venv; 

(* Voce pode testar os dois ao mesmo tempo chamando a 
  funcao run! *)

(* val r = run(abs); *)
