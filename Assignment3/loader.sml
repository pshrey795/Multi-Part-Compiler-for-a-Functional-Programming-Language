CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "AST.sml";
use "A3.yacc.sig";
use "A3.yacc.sml";
use "A3.lex.sml";
use "binder.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)