ML - probleme pt colocviu


fun sumto 0 = 0
|   sumto n = n + sumto(n-1);
 
fun listfrom 0 = []
|   listfrom n = n::listfrom(n-1);
 
fun strcopy(s, 0) = ""
|   strcopy(s, n) = s ^ strcopy(s,n-1);

fun power(x,0) = 1
|   power(x,n) = x*power(x,n-1);
 
fun listcopy(x,0) = []
|   listcopy(x,n) = x::listcopy(x,n-1);
 
fun sumEvens 0 = 0
|   sumEvens n = n + sumEvens(n-2);
 
fun listOdds 1 = [1]
|   listOdds n = n::listOdds(n-2);
 
fun nat 0 = "zero"
|   nat n = "succ(" ^ nat(n-1) ^ ")";
 
fun listTo 0 = nil
|   listTo n = listTo(n-1) @ [n];

val listTo = rev o listfrom;
 
        fun listTo n = let
                fun lt(0,acc) = acc
                |   lt(n,acc) = lt(n-1,n::acc)
                       in lt(n, nil) end;

fun double x = 2*x;
fun triple x = 3*x;
fun times4 x = double(double x);
fun times6 x = double(triple x);
fun times9 x = triple(triple x);
 
fun duplicate s    = s^s;
fun quadricate s   = duplicate(duplicate s);
fun octicate s     = duplicate(quadricate s);
fun hexadecicate s = quadricate(quadricate s);
 
fun middle s   = substring(s, size s div 2,1);
fun dtrunc s   = substring(s, 1, size  s -  2);
fun incFirst s = chr(ord s + 1) ^ substring(s, 1, size s -1);
fun switch s   = substring(s,size s div 2,size s div 2) ^
                 substring(s, 0, size s div 2);
fun dubmid s   = substring(s,0,(1 + size s) div 2) ^
                 substring(s,size s div 2,(1+size s) div 2);

fun len nil   = 0
|   len(h::t) = 1 + len t;

fun triplist nil   = nil
|   triplist(h::t) = 3*h :: triplist t;

fun duplist nil   = nil
|   duplist(h::t) = h::h::duplist t;

fun product nil   = 1
|   product(h::t) = h * product t;

fun vallist nil = nil
|   vallist(h::t) = (ord h - ord "0") :: vallist t;

fun rev nil  = nil
|   rev(h::t)= (rev t) @ [h];

fun space nil   = nil
|   space(h::t) = h::" "::space t;

fun flatten nil  = nil
|   flatten(h::t) = h @ flatten t;

fun count_1's nil    = 0
|   count_1's (1::t) = 1 + count_1's t
|   count_1's (h::t) = count_1's t;

fun timeslist x nil   = nil
|   timeslist x(h::t) = (x*h : int)::timeslist x t;

fun last(h::nil)= h
|   last(h::t)  = last t;

fun member x  nil  = false
|   member x(h::t) = (x=h) orelse member x t;

fun	index(0, h::t) 	= h
|	index(n, h::t) 	= index(n-1, t);

fun	takeN(0, h::t) 	= nil
|	takeN(n, h::t) 	= h :: takeN(n-1, t);

fun	dropN(0, x) 	= x
|	dropN(n, h::t)	= dropN(n-1,t);

fun insert (n:int) nil = [n]
|   insert n (h::t)    = if (n<h) then n::h::t else h::insert n t;

fun sort nil = nil
|   sort(h::t) = insert h (sort t);

fun upto m n = if n<m then nil else m::upto (m+1) n;


fun     unfair(P,r) = r
|       unfair(x++q,r) = x++unfair(q,r);

fun     doomsday(P) = P
|       doomsday(q) = front q ++ doomsday(remove q);

fun     rude(pushy,P) = pushy++P
|       rude(pushy,x++q) = x++rude(pushy,q);

fun     coup q = front q ++ remove q;

fun     nthq(q, 0) = P
|       nthq(q, n) = rude(front q, nthq(remove q,n-1));

fun     l2q nil = P
|       l2q(h::t) = h++l2q t;

fun     q2l P = nil
|       q2l(x++q) = x::q2l q;

fun     fair(q, P) = q
|       fair(P, q) = q
|       fair(q, q') = rude(front q',rude(front q,fair(remove q,remove q')));


TREES:

datatype 'a tree = empty | node of 'a * 'a tree * 'a tree;

fun nodes (empty) = 0
  | nodes (node (_, t1, t2)) = 1 + nodes t1 + nodes t2;

fun maxdepth (empty) = 0
  | maxdepth (node (_, t1, t2)) =
    1 + Int.max (maxdepth t1, maxdepth t2);


QUEUES:

infixr 5 ++;
datatype 'a queue = P | ++ of 'a * 'a queue;
fun     front(x++P) = x
|       front(x++q) = front q;
fun     remove(x++P) = P
|       remove(x++q) = x++(remove q);

