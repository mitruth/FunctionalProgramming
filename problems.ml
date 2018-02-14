fun sumto 0 = 0
|   sumto n = n + sumto(n-1);


(* replaceAll "abcd asdf" #"a" #"c" = "cbcd csdf"
 replaceAll "aabbccab" #"a" #"x" = "xxbbccxb" *)
fun replaceAll nil (c1:char) (c2:char) = nil
|   replaceAll (h::t) (c1:char) (c2:char) = if (h=c1) then c2::(replaceAll t c1 c2) else h::(replaceAll t c1 c2);
 
fun replace s (c1:char) (c2:char) = implode(replaceAll (explode s) c1 c2);


(* drop the elements from positions matching power of 2
 drop [1,2,3,4,5,6] = [3,5,6]
 drop [1,2,3,4,5,6,7,8,9,10] = [3,5,6,7,9,10]*)
fun drop l = let
	fun pow2 1 = true
	|   pow2 x = if (x mod 2 <> 0) then false else pow2(x div 2)
	fun dropIt (nil, n) = nil
	|   dropIt ((h::t), n) = if (pow2(n) = true) then dropIt(t, n+1) else h::dropIt(t, n+1)
in dropIt(l, 1) end;


(* sum all the value from a tree
 sum(T(1, L, L)) = 1
 sum(T(10, T(6, T(3,L,L), L), T(13, T(11, L, L), T(15, L, L)))) *)
datatype 'a tree = L | T of 'a * 'a tree * 'a tree;
fun sum L = 0
|   sum (T(k,L,R)) = k + sum L + sum R;


(* drop every second element from a list
 drop [1,2,3,4] = [1,3]
 drop [1,2,3,4,5] = [1,3,5] *)
fun drop2 nil = nil
|   drop2 (h::_::t) = h::drop2(t);


(* transform the first character of every word from a sentence to the corresponding capital letter
 formatText (" a sample text") = " A Sample Text"
 formatText ("Second test sentence for my function") = "Second Test Sentence For My Function" *)
fun formatText text = let 
		fun smallToCapital c = chr(ord(c) - 32)
		fun transform (nil, pc) = nil
		|   transform ((h::t), pc) =  if (pc = #" ") then smallToCapital(h)::transform(t,h) else h::transform(t,h)
in implode(transform(explode(text), #"!")) end;


(* apply a function f to every element from a list
 my_map inc [1,3,6] = [2,4,7]
 my_map dec [1,3,6] = [0,2,5] *)
fun my_map f nil = nil
|   my_map f (h::t) = f(h)::(my_map f t);

val inc = my_map(fn x => x+1);
val dec = my_map(fn x => x-1);


(* return the string key corresponding to the greatest integer value from a list of pairs(String, Int)
 getMaxValueKey([("A", 2), ("b", 23), ("c", 11)]) = "b"
 getMaxValueKey([("Ab", 2), ("de", 2)]) = "Ab" *)
fun getMaxValueKey l = let
		fun maxPair((k1,v1), (k2,v2)) = if (v1>v2) then (k1, v1) else (k2, v2)
		fun getMaxValue (nil, (k, v)) = k
		|   getMaxValue ((h::t), (k,v)) = getMaxValue(t, maxPair(h, (k, v)))
in getMaxValue(l, (" ", 0)) end;


(* keep only one space character as word delimiter
// formatSentence("Test  sentence for  this   function") = "Test sentence for this function"
// formatSentence(" Test  sentence for  this   function  ") = "Test sentence for this function"*)
fun formatSentence text = let
		fun dropSpaces nil = nil
		|   dropSpaces (h::t) = if (h <> #" ") then h::t else dropSpaces(t);


(*return the list of string keys from a list of pairs(String, Int)
// getKeys([("A", 2), ("b", 23), ("c", 11)]) = ["A", "b", "c"]
// getKeys([("Ab", 2)]) = ["Ab"] *)
fun getKeys nil = nil
|   getKeys ((k,v)::t) = k::getKeys(t);


(*return the first word
// firstWord("Test data") = "Test"
// firstWord(" Test data 2") = "Test" *)
fun firstWord text = let
		fun dropSpaces nil = nil
		|   dropSpaces (h::t) = if (h <> #" ") then h::t else dropSpaces(t);
		fun getWord (nil, acc) = nil
		|   getWord ((h::t), acc) = if (h = #" ") then acc else getWord(t, acc@[h]);
in implode(getWord(dropSpaces(explode(text)), nil)) end;


(*SUMLISTS *)
fun sumlists nil nil = nil
|   sumlists (h::t) nil = h::(sumlists t nil)
|   sumlists nil (h::t) = h::(sumlists nil t)
|   sumlists (h1::t1) (h2::t2) = (h1+h2)::(sumlists t1 t2);


sumlists2 nil nil = nil 
| sumlists2 h::t nil = h::sumlists2 t nil 
| sumlists2 nil h::t = h::sumlists2 nil t 
| sumlists2 (h1::t1) (h2::t2) = (h1+h2):: sumlists2 t1 t2;


(* zip shit*)
fun zip _ _ nil _ = nil 
| zip _ _ _ nil = nil 
| zip f g (h1::t1) (h2::t2) = (f h1, g h2)::(zip f g t1 t2);



infixr 5 ++;

val lista = 1++2++3++4++P;

datatype 'a linkedList = E of 'a | N of 'a * 'a linkedList;


fun middle2 l = let
	fun length E(_) = 1
	|   length B(_, b) = 1 + len(b);
	fun realMiddle(B(nr, x), index, size) = if (index = size div 2) then nr else realMiddle(x,index+1,size)
in realMiddle(l, 0, length(l)) end;


fun middle l = let 
		fun length P = 0
		|   length (h++t) = 1+length(t);
		fun findMiddle (x++q, acc) = if (acc=length(q) div 2) then x else findMiddle(q, acc + 1)
in findMiddle(l, 0) end; *)

datatype 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;

fun treeToList Leaf = Leaf
|   treeToList (Node (t1, t2)) = treeToList t1 :: treeToList t2;




