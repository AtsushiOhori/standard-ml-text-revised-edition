smlsharp<<EOF
structure IntQueue = struct
exception EmptyQueue
type queue = int list ref
fun newQueue() = ref nil : queue
fun enqueue (item,queue) = 
    queue := item :: (!queue)
fun removeLast nil = raise EmptyQueue
  | removeLast [x] = (nil,x)
  | removeLast (h::t) =
    let val (t',last) = removeLast t
    in (h::t',last)
    end
fun dequeue queue =
    let val (rest,last) = removeLast (!queue)
    in (queue:=rest; last)
    end
end;
val q = IntQueue.newQueue();
app (fn x => IntQueue.enqueue(x,q)) [1,3,5];
IntQueue.dequeue q;

open IntQueue;
dequeue q;

structure FastIntQueue = struct
exception EmptyQueue
type queue = int list ref * int list ref
fun newQueue () = (ref [],ref []) : queue
fun enqueue (i,(a,b))  = a := i :: (!a)
fun dequeue (ref [],ref []) = raise EmptyQueue
  | dequeue (a as ref L, b as ref []) = 
    let val (h::t) = rev L
    in (a:=nil; b:=t; h)
    end
  | dequeue (a,b as ref (h::t)) = (b := t; h)
end;

signature QUEUE = sig
  exception EmptyQueue
  type queue
  val newQueue : unit -> queue
  val enqueue : int*queue -> unit
  val dequeue : queue -> int
end;

structure IntQueue :QUEUE = IntQueue;
val q = IntQueue.newQueue();
IntQueue.enqueue (1,q);
IntQueue.dequeue q;

IntQueue.enqueue (2,q);
q := [1];
IntQueue.dequeue q;

structure AbsIntQueue :> QUEUE = IntQueue;

val q = AbsIntQueue.newQueue();
AbsIntQueue.enqueue (1,q);
AbsIntQueue.dequeue q;
q := [1];


signature POLY_QUEUE = sig
  exception EmptyQueue
  type elem
  type queue
  val newQueue : unit -> queue
  val enqueue : elem*queue -> unit
  val dequeue : queue -> elem
end;

structure CQueue :> POLY_QUEUE where type elem = char =
struct
  exception EmptyQueue
  type elem = char
  type queue = elem list ref
  fun newQueue() = ref nil : queue
  fun enqueue (item,queue) = 
      queue := item :: (!queue)
  fun removeLast nil = raise EmptyQueue
    | removeLast [x] = (nil,x)
    | removeLast (h::t) =
      let val (t',last) = removeLast t
      in (h::t',last)
      end
  fun dequeue queue =
      let val (rest,last) = removeLast (!queue)
      in (queue:=rest; last)
      end
end;

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

structure STQueue :> POLY_QUEUE where type elem = string tree = 
struct
  type elem = string tree
  type queue = elem list ref * elem list ref
  exception EmptyQueue
  fun newQueue () = (ref [],ref []) : queue
  fun enqueue (i,(a,b))  = a := i :: (!a)
  fun dequeue (ref [],ref []) = raise EmptyQueue
    | dequeue (a as ref L, b as ref []) = 
      let val (h::t) = rev L
      in (a:=nil; b:=t; h)
      end
    | dequeue (a,b as ref (h::t)) = (b := t; h)
end;

structure BF = 
struct
  structure Q = STQueue
  fun bf t =
    let val queue = Q.newQueue()
        fun loop () = 
            (case Q.dequeue queue of
               Node(data,l,r) => (Q.enqueue (l,queue); 
                                  Q.enqueue (r,queue); 
                                  data::loop())
             | Empty => loop())
            handle Q.EmptyQueue => nil
    in (Q.enqueue (t,queue); loop())
    end
end;

functor QueueFUN(type ty) :> POLY_QUEUE where type elem = ty = 
struct
  exception EmptyQueue
  type elem = ty
  type queue = elem list ref * elem list ref
  fun newQueue () = (ref [],ref []) : queue
  fun enqueue (i,(a,b))  = a := i :: (!a)
  fun dequeue (ref [],ref []) = raise EmptyQueue
    | dequeue (a as ref L, b as ref []) = 
      let val (h::t) = rev L
      in (a:=nil; b:=t; h)
      end
    | dequeue (a,b as ref (h::t)) = (b := t; h)
end;

signature BUFFER = sig
  exception EndOfBuffer
  type channel 
  val openBuffer : unit -> channel
  val input : channel -> char
  val output : channel * char -> unit
end
;

functor BufferFUN(structure CQueue : POLY_QUEUE
                                     where type elem = char) 
        :> BUFFER =
struct
  exception EndOfBuffer
  type channel = CQueue.queue
  fun openBuffer () = CQueue.newQueue()
  fun input ch = CQueue.dequeue ch 
      handle CQueue.EmptyQueue => raise EndOfBuffer
  fun output(ch,c) = CQueue.enqueue (c,ch)
end
structure CQueue = QueueFUN(type ty = char);
structure Buffer = BufferFUN(structure CQueue = CQueue);
EOF
