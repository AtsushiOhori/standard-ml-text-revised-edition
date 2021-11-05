smlsharp<<EOF
fun timeRun f x =
    let
      val timer = Timer.startCPUTimer()
      val _ = f x
      val tm = Timer.checkCPUTimer timer
      val ut = Time.toMicroseconds (#usr tm)
    in LargeInt.toInt ut
    end

structure Rand =
struct
  val rand = _import "rand" : () -> int
  val srand = _import "srand" : word -> ()
  fun randInit () =
      srand (Word.fromLargeInt 
             ((Time.toSeconds (Time.now())) mod 0x7FFFFFFF))
end

fun genArray n = Array.tabulate(n,fn x => Rand.rand())

fun log2 x = Math.log10 x / (Math.log10 2.0)
fun nlogn n = ((Real.fromInt n) * (log2 (Real.fromInt n)))

signature SORT = 
sig
 val sort : 'a array * ('a * 'a -> order) -> unit
end

structure ArrayQuickSort : SORT  = 
struct
local
  open Array
in
fun sort (array,comp) =
    let
      fun swap (i,j) =
          let val temp = sub(array,i)
          in (update(array,i,sub(array,j)); update(array,j,temp))
          end
      fun getPivot (i,j) =
          let
            val delta = (j-i) div 4
            val i = i + delta
            val j = i + delta * 3
            val mid = i + (j-i) div 2
            val ie = sub(array,i)
            val je = sub(array,j)
            val me = sub(array,mid)
          in
            case (comp(ie,me),comp(me, je))
             of (LESS, LESS) => (mid,me)
              | (LESS, _) => (case comp(ie, je) of LESS => (j,je) | _ => (i,ie))
              | (_, GREATER) => (mid,me)
              | _ => (case comp(ie, je) of LESS => (i,ie) | _ => (j,je))
          end
      fun qsort (i,j) =
          if j <= i+1 then ()
          else 
            let 
              val pivot = 
                  let val (pi,pivot) = getPivot(i,j-1) 
                  in update(array,pi,sub(array,i));
                     update(array,i,pivot);
                     pivot
                  end
              fun partition (a,b) =
                  if b < a then (a - 1)
                  else
                    let
                      fun scanRight a = 
                          if a > b then a
                          else  
                            case comp(sub(array,a),pivot) of
                              GREATER => a
                            | _ => scanRight (a+1)
                      val a = scanRight a
                      fun scanLeft b = 
                          if b < a then b
                          else 
                            case comp(sub(array,b),pivot) of
                              GREATER => scanLeft (b - 1)
                            | _ => b
                      val b = scanLeft b
                    in
                      if b < a then (a - 1)
                      else (swap(a,b);partition (a+1,b-1))
                    end
              val k = partition (i+1,j-1)
              val _ =  swap(i,k)
            in
              (qsort (i,k); qsort (k+1,j))
            end
    in
      qsort (0,Array.length array)
    end
end
end;

fun checkTime n = 
    let
      val array = genArray n
      val tm = timeRun ArrayQuickSort.sort (array,Int.compare)
      val nlognRatio = Real.fromInt tm / (nlogn n)
    in
      (n, tm div 1000, nlognRatio)
    end

fun printResult (n, tm, ratio) =
    Dynamic.pp
      {"size" = n, "milli-secs" = tm, "micro-secs/n long(n)" = ratio}
fun testSort n = printResult (checkTime n)
EOF
