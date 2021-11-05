smlsharp<<EOF
fun decScan x = Int.scan StringCvt.DEC x;
val intScan = decScan Substring.getc;
val s = Substring.full "123 abc";
intScan s;
signature PARSE_URL = sig
  exception urlFormat
  datatype url = 
           HTTP of {host:string list, path:string list option,
                    anchor:string option}
         | FILE of {path:string list, anchor:string option}
         | RELATIVE of {path:string list, anchor:string option}
  val parseUrl : string -> url
end
;
structure ParseURL:PARSE_URL = struct
  structure S = Substring
  exception urlFormat
  datatype url
    = HTTP of {host : string list, path : string list option,
               anchor : string option}
    | FILE of {path : string list, anchor : string option}
    | RELATIVE of {path : string list, anchor : string option}
fun parseHttp s = 
  let val s = if S.isPrefix "://" s then 
                S.triml 3 s 
               else raise urlFormat
      fun neq c x = not (x = c)
      fun eq c x = c = x
      val (host,body) = S.splitl (neq #"/") s
      val domain = map S.string (S.tokens (eq #".") host)
      val (path,anchor) = 
          if S.isEmpty body then (NONE,NONE)
          else 
            let val (p,a) = S.splitl (neq #"#") body
            in (SOME (map S.string (S.tokens (eq #"/") p)), 
                if S.isEmpty a then NONE 
                else SOME (S.string (S.triml 1 a)))
            end
  in {host=domain, path=path, anchor=anchor}
  end
fun parseFile s = 
  let val s = if S.isPrefix ":/" s then S.triml 2 s else raise urlFormat
      val (path,anchor) = 
          let val (p,a) = S.splitl (fn #"#" => false | _ => true) s
          in (map S.string (S.tokens (fn c => c = #"/") p), 
              if S.isEmpty a then NONE else SOME (S.string (S.triml 1 a)))
          end
  in  {path=path,anchor=anchor}
  end
fun parseRelative s = 
  let val (path,anchor) = 
          let val (p,a) = S.splitl (fn #"#" => false | _ => true) s
          in (map S.string (S.fields (fn c => c = #"/") p), 
              if S.isEmpty a then NONE else SOME (S.string (S.triml 1 a)))
          end
  in {path=path, anchor=anchor}
  end
  fun parseUrl s = 
      let val s = S.full s
          val (scheme,body) = S.splitl (fn c => not (c = #":")) s
      in
        if S.isEmpty body then 
          RELATIVE (parseRelative scheme)
        else case S.string scheme of 
               "http" => HTTP (parseHttp body)
             | "file" => FILE (parseFile body)
             | _  => raise urlFormat
      end
end
;
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
;
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
end;
EOF
