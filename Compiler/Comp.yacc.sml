functor CompLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Comp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\007\000\013\000\
\\008\000\012\000\011\000\011\000\018\000\010\000\025\000\009\000\
\\028\000\008\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\103\000\
\\012\000\103\000\013\000\103\000\014\000\103\000\015\000\103\000\
\\016\000\103\000\017\000\103\000\018\000\010\000\019\000\024\000\
\\020\000\023\000\021\000\022\000\024\000\103\000\026\000\103\000\
\\027\000\103\000\029\000\103\000\030\000\103\000\031\000\103\000\
\\033\000\103\000\034\000\007\000\035\000\103\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\104\000\
\\012\000\104\000\013\000\104\000\014\000\104\000\015\000\104\000\
\\016\000\104\000\017\000\104\000\018\000\010\000\019\000\024\000\
\\020\000\023\000\021\000\022\000\024\000\104\000\026\000\104\000\
\\027\000\104\000\029\000\104\000\030\000\104\000\031\000\104\000\
\\033\000\104\000\034\000\007\000\035\000\104\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\027\000\064\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\029\000\053\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\030\000\063\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\031\000\075\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\033\000\052\000\034\000\007\000\000\000\
\\001\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\034\000\007\000\000\000\
\\001\000\001\000\034\000\000\000\
\\001\000\001\000\038\000\000\000\
\\001\000\001\000\056\000\000\000\
\\001\000\001\000\062\000\000\000\
\\001\000\004\000\068\000\005\000\067\000\034\000\066\000\000\000\
\\001\000\006\000\061\000\000\000\
\\001\000\006\000\069\000\000\000\
\\001\000\006\000\076\000\000\000\
\\001\000\006\000\082\000\000\000\
\\001\000\009\000\017\000\000\000\
\\001\000\010\000\072\000\033\000\071\000\000\000\
\\001\000\010\000\072\000\033\000\078\000\000\000\
\\001\000\010\000\072\000\033\000\079\000\000\000\
\\001\000\026\000\054\000\000\000\
\\001\000\032\000\055\000\000\000\
\\001\000\034\000\037\000\000\000\
\\001\000\034\000\057\000\000\000\
\\001\000\035\000\000\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\012\000\030\000\013\000\029\000\014\000\028\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\010\000\019\000\024\000\
\\020\000\023\000\021\000\022\000\022\000\021\000\023\000\020\000\
\\024\000\019\000\034\000\007\000\000\000\
\\089\000\001\000\016\000\002\000\015\000\003\000\014\000\007\000\013\000\
\\008\000\012\000\011\000\011\000\018\000\010\000\025\000\009\000\
\\028\000\008\000\034\000\007\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\019\000\024\000\020\000\023\000\021\000\022\000\
\\022\000\021\000\023\000\020\000\034\000\007\000\000\000\
\\095\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\019\000\024\000\020\000\023\000\021\000\022\000\
\\022\000\021\000\023\000\020\000\034\000\007\000\000\000\
\\096\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\019\000\024\000\020\000\023\000\021\000\022\000\
\\022\000\021\000\023\000\020\000\034\000\007\000\000\000\
\\097\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\019\000\024\000\020\000\023\000\021\000\022\000\
\\022\000\021\000\023\000\020\000\034\000\007\000\000\000\
\\098\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\018\000\010\000\019\000\024\000\020\000\023\000\021\000\022\000\
\\022\000\021\000\023\000\020\000\034\000\007\000\000\000\
\\099\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\034\000\007\000\000\000\
\\100\000\001\000\016\000\002\000\015\000\003\000\014\000\018\000\010\000\
\\021\000\022\000\034\000\007\000\000\000\
\\101\000\001\000\016\000\002\000\015\000\003\000\014\000\018\000\010\000\
\\021\000\022\000\034\000\007\000\000\000\
\\102\000\001\000\016\000\002\000\015\000\003\000\014\000\018\000\010\000\
\\034\000\007\000\000\000\
\\105\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\034\000\007\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\010\000\072\000\000\000\
\\113\000\010\000\072\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\010\000\072\000\000\000\
\\117\000\010\000\072\000\000\000\
\\118\000\010\000\081\000\000\000\
\\119\000\001\000\016\000\002\000\015\000\003\000\014\000\011\000\011\000\
\\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\010\000\019\000\024\000\020\000\023\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\024\000\019\000\
\\034\000\007\000\000\000\
\"
val actionRowNumbers =
"\030\000\033\000\018\000\029\000\
\\027\000\000\000\000\000\009\000\
\\008\000\008\000\024\000\010\000\
\\047\000\046\000\045\000\000\000\
\\034\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\030\000\007\000\004\000\022\000\
\\023\000\048\000\049\000\011\000\
\\025\000\044\000\040\000\002\000\
\\001\000\043\000\042\000\041\000\
\\039\000\038\000\037\000\036\000\
\\035\000\028\000\050\000\000\000\
\\000\000\000\000\014\000\012\000\
\\005\000\003\000\058\000\013\000\
\\015\000\000\000\032\000\019\000\
\\013\000\054\000\053\000\013\000\
\\006\000\016\000\013\000\020\000\
\\021\000\031\000\013\000\055\000\
\\057\000\017\000\052\000\013\000\
\\013\000\056\000\051\000\026\000"
val gotoT =
"\
\\001\000\083\000\002\000\004\000\003\000\003\000\005\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\000\000\
\\000\000\
\\003\000\029\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\030\000\005\000\002\000\007\000\001\000\000\000\
\\004\000\031\000\000\000\
\\007\000\033\000\000\000\
\\007\000\034\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\037\000\005\000\002\000\007\000\001\000\000\000\
\\000\000\
\\003\000\038\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\039\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\040\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\041\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\042\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\043\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\044\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\045\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\046\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\047\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\048\000\005\000\002\000\007\000\001\000\000\000\
\\002\000\049\000\003\000\003\000\005\000\002\000\007\000\001\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\003\000\056\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\057\000\005\000\002\000\007\000\001\000\000\000\
\\003\000\058\000\005\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\007\000\016\000\000\000\
\\006\000\063\000\000\000\
\\000\000\
\\003\000\068\000\005\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\071\000\000\000\
\\000\000\
\\000\000\
\\006\000\072\000\000\000\
\\007\000\016\000\000\000\
\\000\000\
\\006\000\075\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\081\000\000\000\
\\006\000\082\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 84
val numrules = 34
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | CONST of unit ->  (string)
 | ID of unit ->  (string) | VAR of unit ->  (AST.exp)
 | TYP of unit ->  (AST.typ) | FUNDECL of unit ->  (AST.fundecl)
 | DECL of unit ->  (AST.decl) | EXP of unit ->  (AST.exp)
 | PROGRAM of unit ->  (AST.program) | FILE of unit ->  (AST.program)
end
type svalue = MlyValue.svalue
type result = AST.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 34) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "CONST"
  | (T 2) => "NUM"
  | (T 3) => "INT"
  | (T 4) => "BOOL"
  | (T 5) => "COLON"
  | (T 6) => "FUN"
  | (T 7) => "FN"
  | (T 8) => "DEC"
  | (T 9) => "ARROW"
  | (T 10) => "NOT"
  | (T 11) => "TERM"
  | (T 12) => "AND"
  | (T 13) => "OR"
  | (T 14) => "XOR"
  | (T 15) => "EQUALS"
  | (T 16) => "NEQUALS"
  | (T 17) => "NEGATE"
  | (T 18) => "PLUS"
  | (T 19) => "MINUS"
  | (T 20) => "TIMES"
  | (T 21) => "GREATERTHAN"
  | (T 22) => "LESSTHAN"
  | (T 23) => "IMPLIES"
  | (T 24) => "LET"
  | (T 25) => "IN"
  | (T 26) => "END"
  | (T 27) => "IF"
  | (T 28) => "THEN"
  | (T 29) => "ELSE"
  | (T 30) => "FI"
  | (T 31) => "ASSN"
  | (T 32) => "RPAREN"
  | (T 33) => "LPAREN"
  | (T 34) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, 
PROGRAM1right)) :: rest671)) => let val  result = MlyValue.FILE (fn _
 => let val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (PROGRAM)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM1, _, PROGRAM1right)) :: _ ::
 ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  
result = MlyValue.PROGRAM (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (AST.addExp(EXP,PROGRAM))
end)
 in ( LrTable.NT 1, ( result, EXP1left, PROGRAM1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.PROGRAM (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (AST.addExp(EXP,AST.ExpList([])))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.PROGRAM (fn _ => (
AST.ExpList([])))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _))
 :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP 
EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (AST.ConditionExp(EXP1,EXP2,EXP3))
end)
 in ( LrTable.NT 2, ( result, IF1left, FI1right), rest671)
end
|  ( 5, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _))
 :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.LetExp(DECL,EXP))
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.VAR VAR1, VAR1left, VAR1right)) :: rest671))
 => let val  result = MlyValue.EXP (fn _ => let val  (VAR as VAR1) = 
VAR1 ()
 in (VAR)
end)
 in ( LrTable.NT 2, ( result, VAR1left, VAR1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (VAR as VAR1) = VAR1 ()
 in (AST.AppExp(EXP,VAR))
end)
 in ( LrTable.NT 2, ( result, EXP1left, VAR1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.BoolOp(AST.And),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.BoolOp(AST.Or),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.BoolOp(AST.Xor),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Equals,EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Nequals,EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.BoolOp(AST.Implies),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.IntOp(AST.Add),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.IntOp(AST.Sub),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.IntOp(AST.Times),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.RelOp(AST.GreaterThan),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.RelOp(AST.LessThan),EXP1,EXP2))
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.FUNDECL FUNDECL1, FUNDECL1left, _)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => let val  (FUNDECL as FUNDECL1) = 
FUNDECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.DeclExp(FUNDECL,EXP))
end)
 in ( LrTable.NT 2, ( result, FUNDECL1left, EXP1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.VAR (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.VAR (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (if CONST="TRUE" then AST.BoolExp(true) else AST.BoolExp(false))

end)
 in ( LrTable.NT 6, ( result, CONST1left, CONST1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.VAR (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.IntExp(NUM))
end)
 in ( LrTable.NT 6, ( result, NUM1left, NUM1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.VAR (fn _
 => let val  (VAR as VAR1) = VAR1 ()
 in (AST.UnaryExp(AST.Negate,VAR))
end)
 in ( LrTable.NT 6, ( result, NEGATE1left, VAR1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.VAR (fn _ =>
 let val  (VAR as VAR1) = VAR1 ()
 in (AST.UnaryExp(AST.Not,VAR))
end)
 in ( LrTable.NT 6, ( result, NOT1left, VAR1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.VAR (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.TYP TYP2, _, TYP2right)) :: _ :: _ :: ( _, 
( MlyValue.TYP TYP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.FUNDECL (fn _ => let val  ID1
 = ID1 ()
 val  ID2 = ID2 ()
 val  TYP1 = TYP1 ()
 val  TYP2 = TYP2 ()
 in (AST.Fun(ID1,ID2,TYP1,TYP2))
end)
 in ( LrTable.NT 4, ( result, FUN1left, TYP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.TYP TYP2, _, TYP2right)) :: _ :: _ :: ( _, 
( MlyValue.TYP TYP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) ::
 _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = 
MlyValue.FUNDECL (fn _ => let val  ID1 = ID1 ()
 val  TYP1 = TYP1 ()
 val  TYP2 = TYP2 ()
 in (AST.Fn(ID1,TYP1,TYP2))
end)
 in ( LrTable.NT 4, ( result, FN1left, TYP2right), rest671)
end
|  ( 28, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TYP (fn _ => (AST.INT))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 29, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TYP (fn _ => (AST.BOOL))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.TYP TYP2, _, TYP2right)) :: _ :: ( _, ( 
MlyValue.TYP TYP1, TYP1left, _)) :: rest671)) => let val  result = 
MlyValue.TYP (fn _ => let val  TYP1 = TYP1 ()
 val  TYP2 = TYP2 ()
 in (AST.Arrow(TYP1,TYP2))
end)
 in ( LrTable.NT 5, ( result, TYP1left, TYP2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.TYP TYP2, _, TYP2right)) :: _ :: _ :: ( _, 
( MlyValue.TYP TYP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671))
 => let val  result = MlyValue.TYP (fn _ => let val  TYP1 = TYP1 ()
 val  TYP2 = TYP2 ()
 in (AST.Arrow(TYP1,TYP2))
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, TYP2right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.TYP TYP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.TYP (fn _ => let val  (TYP as TYP1) = TYP1 ()
 in (TYP)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.ValDecl(ID,EXP))
end)
 in ( LrTable.NT 3, ( result, ID1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.FILE x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Comp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
