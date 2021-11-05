smlsharp<<EOF
open TextIO;
fun copyStream ins outs = 
    if endOfStream ins then ()
    else case input1 ins of
           SOME c => (output1(outs,c);
                      copyStream ins outs)
         | NONE => copyStream ins outs
fun copyFile inf outf =
    let val ins = openIn inf
        val outs = openOut outf
    in (copyStream ins outs;
        closeIn ins; closeOut outs)
    end

fun filterStream f ins outs = 
    if endOfStream ins then ()
    else case input1 ins of
           SOME c => (output1(outs,f c);
                      filterStream f ins outs)
         | NONE => filterStream f ins outs
fun filterFile f inf outf =
    let val ins = openIn inf
        val outs = openOut outf
    in (filterStream f ins outs;
        closeIn ins; closeOut outs)
    end

fun echo () =
    let
      fun loop () =
          (print "? ";
           if TextIO.endOfStream TextIO.stdIn then ()
           else
             case TextIO.inputLine TextIO.stdIn of 
               SOME string => (print string; loop ())
             | NONE => loop()
          )
    in
      loop()
    end;
type instream = StreamIO.instream ref;
fun input1 s =
 case StreamIO.input1 (!s) of
   SOME (a,newS) =>(s:=newS; SOME a)
 | NONE => NONE;

signature ADVANCED_IO =
sig
  type instream
  type outstream
  val openIn : string -> instream
  val openOut : string -> outstream
  val inputN :  instream * int -> string
  val lookAheadN : instream * int -> string
  val endOfStream : instream -> bool
  val check : instream -> unit
  val reset : instream -> unit
  val output : outstream * string -> unit
  val redirectIn : instream * instream -> unit
  val redirectOut : outstream * outstream -> unit
end;

structure AdvancedIO :> ADVANCED_IO =  
struct
  structure T = TextIO
  structure S = TextIO.StreamIO
  type instream = T.instream * S.instream ref
  type outstream = T.outstream 
  fun openIn f = let val s = T.openIn f
                 in (s,ref (T.getInstream s))
                 end
  fun inputN ((s,_),n) = T.inputN (s,n)
  fun lookAheadN ((s,_),n) = let val ss = T.getInstream s
                             in #1 (S.inputN (ss,n))
                             end
  fun endOfStream(s,_) = T.endOfStream s
  fun check (s,ss) = ss := T.getInstream s
  fun reset (s,ref ss) = T.setInstream (s,ss)
  fun redirectIn ((s1,_),(s2,_)) = 
      T.setInstream(s1,T.getInstream s2)
  fun redirectOut (s1,s2) = T.setOutstream(s1,T.getOutstream s2)
  val openOut = T.openOut
  val output = T.output
end;
structure T = TextIO;
datatype token  
  = EOF                          | ID of string                 
  | DIGITS of string             | SPECIAL of char              
  | BANG            (* ! *)      | DOUBLEQUOTE     (* " *)      
  | HASH            (* # *)      | DOLLAR          (* $ *)
  | PERCENT         (* % *)      | AMPERSAND       (* & *)
  | QUOTE           (* ' *)      | LPAREN          (* ( *)
  | RPAREN          (* ) *)      | TILDE           (* ~ *)
  | EQUALSYM        (* = *)      | HYPHEN          (* - *)
  | HAT             (* ^ *)      | UNDERBAR        (* _ *)
  | SLASH           (* \ *)      | BAR             (* | *)
  | AT              (* @ *)      | BACKQUOTE
  | LBRACKET        (* [ *)      | LBRACE          (* { *)
  | SEMICOLON       (* ; *)      | PLUS            (* + *)
  | COLON           (* : *)      | ASTERISK        (* * *)
  | RBRACKET        (* ] *)      | RBRACE          (* } *)
  | COMMA           (* , *)      | LANGLE          (* < *)
  | PERIOD          (* . *)      | RANGLE          (* > *)
  | BACKSLASH       (* / *)      | QUESTION        (* ? *)
;
fun skipSpaces ins =
    case T.lookahead ins of
      SOME c => if Char.isSpace c 
                then (T.input1 ins;skipSpaces ins)
                else ()
    | _ => ()
;
fun getID ins =
    let fun getRest s = 
            case T.lookahead ins of
              SOME c => if Char.isAlphaNum c then 
                          getRest (s ^ T.inputN(ins,1))
                        else s
            | _ => s
    in ID(getRest "")
    end
;
fun getNum ins =
    let
      fun getRest s = 
          case T.lookahead ins of
            NONE => s
          | SOME c => 
            if Char.isDigit c then
              getRest (s ^ T.inputN(ins,1))
            else s
    in
      DIGITS (getRest "")
    end
;
fun lex ins =
    (skipSpaces ins;
     if T.endOfStream ins then EOF
     else
       let
         val c = valOf (T.lookahead ins)
       in
         if Char.isDigit c then getNum ins
         else if Char.isAlpha c then getID ins
         else case valOf (T.input1 ins) of
                #"!" => BANG
              | #"\"" => DOUBLEQUOTE
              | #"#" => HASH 
              | #"$" => DOLLAR 
              | #"%" => PERCENT 
              | #"&" => AMPERSAND 
              | #"'" => QUOTE 
              | #"(" => LPAREN 
              | #")" => RPAREN 
              | #"~" => TILDE 
              | #"=" => EQUALSYM
              | #"-" => HYPHEN
              | #"^" => HAT 
              | #"_" => UNDERBAR 
              | #"|" => BAR 
              | #"@" => AT 
              | #"[" => LBRACKET 
              | #"{" => LBRACE 
              | #";" => SEMICOLON 
              | #"+" => PLUS 
              | #":" => COLON 
              | #"*" => ASTERISK 
              | #"]" => RBRACKET 
              | #"}" => RBRACE 
              | #"," => COMMA 
              | #"<" => LANGLE 
              | #"." => PERIOD 
              | #">" => RANGLE 
              | #"/" => BACKSLASH 
              | #"?" => QUESTION 
              | _ => SPECIAL c
       end)
;
fun toString tok =
    case tok of
      EOF => "EOF"
    | ID s => "ID (" ^ s ^ ")"
    | DIGITS s => "DIGITS (" ^ s ^ ")"
    | SPECIAL c => "SPECIAL" ^ Char.toString c ^ ")"
    | BANG => "BANG"  
    | DOUBLEQUOTE => "DOUBLEQUOTE" 
    | HASH => "HASH" 
    | DOLLAR => "DOLLAR" 
    | PERCENT => "PERCENT" 
    | AMPERSAND => "AMPERSAND" 
    | QUOTE => "QUOTE" 
    | LPAREN => "LPAREN" 
    | RPAREN => "RPAREN" 
    | TILDE => "TILDE" 
    | EQUALSYM => "EQUALSYM" 
    | HYPHEN => "HYPHEN" 
    | HAT => "HAT" 
    | UNDERBAR => "UNDERBAR" 
    | SLASH => "SLASH" 
    | BAR => "BAR" 
    | AT => "AT" 
    | BACKQUOTE => "BACKQUOTE" 
    | LBRACKET => "LBRACKET" 
    | LBRACE => "LBRACE" 
    | SEMICOLON => "SEMICOLON" 
    | PLUS => "PLUS" 
    | COLON => "COLON" 
    | ASTERISK => "ASTERISK" 
    | RBRACKET => "RBRACKET" 
    | RBRACE => "RBRACE" 
    | COMMA => "COMMA" 
    | LANGLE => "LANGLE" 
    | PERIOD => "PERIOD" 
    | RANGLE => "RANGLE" 
    | BACKSLASH => "BACKSLASH" 
    | QUESTION => "QUESTION" 
;
fun testLex () =
    let
      val token = lex (TextIO.stdIn)
    in
      case token of 
        EOF => ()
      | _ => (print (toString token ^ "\n");
              testLex ())
    end;
testLex();
dog
(1,2)
EOF
