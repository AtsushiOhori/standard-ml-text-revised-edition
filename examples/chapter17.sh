smlsharp<<EOF
structure F = OS.FileSys;
F.getDir();
F.realPath "../../mltext.tex";
F.fullPath "../../mltext.tex";

fun ls () =
    let val d = F.openDir (F.getDir())
        fun printRest() = 
            case F.readDir d of
              SOME f => (print (f ^ "\n"); printRest())
            | NONE => ()
    in printRest()
    end
;
(*
   signature FORMAT = 
   sig datatype kind =  INT of StringCvt.radix
                      | REAL of StringCvt.realfmt
                      | STRING 
                      | BOOL 
       datatype align = LEFT | RIGHT
       datatype format = 
                LITERAL of string 
              | SPEC of {kind:kind,width:int option,align:align}
       datatype argument = I of int 
                         | R of real 
                         | S of string 
                         | B of bool
       exception formatError
       val format : string -> argument list -> string
       val printf : string -> argument list -> unit
   end
structure Format : FORMAT = 
struct
  exception formatError 
  structure S = Substring
  datatype kind =  INT of StringCvt.radix
                 | REAL of StringCvt.realfmt
                 | STRING 
                 | BOOL 
  datatype align = LEFT | RIGHT
  datatype format = 
           LITERAL of string 
         | SPEC of {kind:kind,width:int option,align:align}
  datatype argument = I of int 
                    | R of real 
                    | S of string 
                    | B of bool
  fun formatData {kind,width,align} data=
      let val body = 
              case (kind,data) of
                (INT radix,I i) => Int.fmt radix i
              | (REAL fmt,R r) => Real.fmt fmt r
              | (STRING,S s) => s
              | (BOOL,B b) => Bool.toString b
              | _ => raise formatError
      in case width of
           NONE => body
         | SOME w => (case align of
                        LEFT => StringCvt.padRight #" " w body
                      | RIGHT => StringCvt.padLeft #" " w body)
      end
  fun scanInt s = 
      let val r= Int.scan StringCvt.DEC S.getc s
      in case r of NONE => (NONE,s)
                 | SOME(n,s) => (SOME n,s)  
      end
  fun oneFormat s = 
      let val s = S.triml 1 s
      in if S.isPrefix "%" s then (LITERAL "%",S.triml 1 s)
         else 
           let val (a,s) = if S.isPrefix "-" s 
                           then (LEFT,S.triml 1 s) 
                           else (RIGHT,s)
               val (w,s) = scanInt s
               val (c,s) = case S.getc s of NONE => raise formatError 
                                          | SOME s  => s
           in (SPEC {width=w,align=a,
                     kind=case c of
                            #"d" => INT StringCvt.DEC
                          | #"s" => STRING
                          | #"f" => REAL (StringCvt.FIX NONE)
                          | #"e" => REAL (StringCvt.SCI NONE)
                          | #"g" => REAL (StringCvt.GEN NONE)
                          | _ => raise formatError},
               s)
           end
      end
  fun parse s = 
      let  
        val (s1,s) = StringCvt.splitl (fn c => c <> #"%") S.getc s
        val prefix = if s1 = "" then nil 
                     else [LITERAL s1]
      in if S.isEmpty s then prefix
         else let val (f,s) = oneFormat s
                  val L = parse s
              in prefix@(f::L)
              end
      end
  fun format s L =
      let val FL = parse (S.full s)
          fun traverse (h::t) L =
              (case h of 
                 LITERAL s => s ^ (traverse t L)
               | SPEC fmt => (formatData fmt (List.hd L) 
                              ^ (traverse t (List.tl L))))
            | traverse nil l = ""
      in (traverse FL L)
      end
  fun printf s L = print (format s L)
end

(* 17.1 ディレクトリとファイルの操作 *)
(* 問 17.1 *)
fun ls () =
    let 
      val d = F.openDir (F.getDir())
      fun printRest () = 
          case F.readDir d of
            NONE => F.closeDir d
          | SOME f => 
            let
              val size = F.fileSize f
              val time = Date.toString (Date.fromTimeLocal (F.modTime f))
              val modString = implode 
                                [if F.isDir f then #"d" else #"-",
                                 if F.isLink f then #"l" else #"-",
                                 if F.access (f, [F.A_READ]) then #"r" else #"-",
                                 if F.access (f, [F.A_WRITE]) then #"w" else #"-",
                                 if F.access (f, [F.A_EXEC]) then #"x" else #"-"]
            in
              (Format.printf
                 "%10s%15d%30s%30s\n"
                 [Format.S modString,
                  Format.I size,
                  Format.S time,
                  Format.S f
                 ];
               printRest())
            end
    in 
      (Format.printf 
         "%10s%15s%30s%30s\n"
         [Format.S "dlrwx",
          Format.S "file size",
          Format.S "last modified",
          Format.S "file name"
         ];
       printRest()
      )
    end
*)
structure F = OS.FileSys;
structure P = OS.Path;
F.getDir();
P.fromString it;
P.toString it;
P.getParent it;
P.splitDirFile it;
P.joinDirFile it ;
P.concat (it,"mltext");
P.mkRelative {path = it, relativeTo = "/home/ohori"};

local
  open TextIO
in
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
end
;
fun copy a b =
    if not (F.isDir a) then 
      copyFile a b
    else
      let val d = F.openDir a
          fun copyDirStream d b =
              case F.readDir d of 
                NONE => F.closeDir d
              | SOME item =>
                let val from = P.concat (a,item)
                    val to = P.concat (b,item)
                in (copy from to;copyDirStream d b)
                end
      in (F.mkDir b; copyDirStream d b)
      end
;
EOF
