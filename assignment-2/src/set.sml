(*
Your name: Tarush Roy
Your student id: V00883469
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

(* assumption: set with empty list is also an empty set *)
fun is_empty_set s =
    case s of
	EmptySet _ => true
      | Set([], _) => true
      | Set(ls, _) => false

(* returns first element of set because set is ordered *)
fun min_in_set s =
    case s of
	EmptySet _ => raise SetIsEmpty
      | Set([], _) => raise SetIsEmpty
      | Set(head::[], _) => head
      | Set(head::tail, _) => head

(* returns last element of a list *)
fun lastElement xs =
    case xs of
	[] => raise SetIsEmpty
      | head::[] => head
      | head::tail => lastElement(tail)

(* returns last element of a set because set is ordered *)
fun max_in_set s =
    case s of
	EmptySet _ => raise SetIsEmpty
      | Set(lst, _) => lastElement(lst)

(* returns bool, recurse on tail of set list till match *)
fun in_set (s, v) =
    case s of
	EmptySet es => false
      | Set([], _) => false
      | Set(head::[], f) => (case f(v, head) of
				 EQUAL => true
			       | _ => false)
      | Set(head::tail, f) => (case f(v, head) of
				   EQUAL => true
				 | _ => in_set(Set(tail, f), v))

(* check if in set, if no, insert into sorted position after comparision *)
fun insert_into_set (s,v) =
    let
	fun insert (lst, v, f) =
	    case lst of
		[] => [v]
	      | head::[] => (case f(v, head) of
				LESS => v::head::[]
			      | EQUAL => head::[]
			      | GREATER => head::v::[])
	      | head::tail => (case f(v, head) of
				  LESS => v::head::tail
				| EQUAL => insert(tail, v, f)
				| GREATER => head::insert(tail, v, f))
    in
	case s of
	    EmptySet es => Set([v], es)
	  | Set(lst, f) => if in_set(s, v)
			   then s
			   else Set(insert(lst, v, f), f)
    end

(* returns list in set *)
fun set_to_list s =
    case s of
	EmptySet es => []
     |  Set(list, _) => list

(* inserts each element of list into a new set and returns that set *)
fun list_to_set (lst, f) =
    let
	fun insert_list (lst, s) =
	    case lst of
		[] => s
	     |  head::[] => insert_into_set(s, head)
	     |  head::tail => insert_list(tail, insert_into_set(s, head))
    in
	insert_list(lst, EmptySet f)
    end

(* returns comparision function of a set *)
fun orderOfSet (s) =
    case s of
	EmptySet es => es
      | Set(_, f) => f
	
(* if element is not matched, add to new set if element matched, do not add to new set *)
fun remove_from_set (s,v) =
    let
	fun delete (lst, f, v, nl) =
	    case lst of
		[] => nl
	      | head::[] => (case f(v, head) of
				 EQUAL => nl
			       | _ => head::nl)
	      | head::tail => (case f(v, head) of
				   EQUAL => delete(tail, f, v, nl)
				 | _ => delete(tail, f, v, head::nl))
    in
	case s of
	    EmptySet es => s
	  | Set(lst, f) => case delete(lst, f, v, []) of
			       [] => EmptySet f
			     | _ => list_to_set(delete(lst, f, v, []), f)
    end

(* using length function to return size *)
fun size_set (s: 'a set) =
    case s of
	EmptySet es => 0
     |  Set(lst, order) => length lst

(* match each element, if fail, return false, else true *)
fun equal_set (s, t) =
    let
	fun equal_lists (l1, l2, f) =
	    case (l1, l2) of
		([], _) => true
	      | (hd1::tl1, hd2::tl2) => (case tl1 of
					    [] => (case f(hd1, hd2) of
						       EQUAL => true
						     | _ => false)
					  | _ => (case f(hd1, hd2) of
						      EQUAL => equal_lists(tl1, tl2, f)
						   |  _ => false))
	      | _ => false
    in
	if size_set(s) = size_set(t)
	then case (s, t) of
		 (EmptySet es1, EmptySet es2) => true
	      |  (Set(lst1, f1), Set(lst2, f2)) => equal_lists(lst1, lst2, f1)
	      |  _ => false
	else false
    end

(* recurse on second set, check if in first set if no, add to first set and return *)
fun union_set (s, t) =
    let
	fun process_list (lst, ns) =
	    case lst of
		[] => ns
	      | head::[] => if in_set(ns, head) then ns else insert_into_set(ns, head)
	      | head::tail => if in_set(ns, head)
			      then process_list(tail, ns)
			      else process_list(tail, insert_into_set(ns, head))
    in
	case (s, t) of
	    (EmptySet s1, EmptySet s2) => s
	  | (EmptySet s1, Set(l2, f2)) => t
	  | (Set(l1, f1), EmptySet s2) => s
	  | (Set(l1, f1), Set(l2, f2)) => process_list(set_to_list(t), s)
    end

(* check if element in second set is in first set, if yes, add to new set *)
fun intersect_set (s, t) =
    let
	fun process_list (ls, ns) =
	    case ls of
		[] => ns
	      | head::[] => if in_set(t, head) then insert_into_set(ns, head) else ns
	      | head::tail => if in_set(t, head)
			      then process_list(tail, insert_into_set(ns, head))
			      else process_list(tail, ns)
    in
	process_list(set_to_list(s), EmptySet(orderOfSet(s)))
    end

(* if element in second set in first set, remove from first set *)
fun except_set (s, t) =
    let
	fun process_list (lst, ns) =
	    case lst of
		[] => ns
	     |  head::[] => if in_set(ns, head) then remove_from_set(ns, head) else ns
	     |  head::tail => if in_set(ns, head)
			      then process_list(tail, remove_from_set(ns, head))
			      else process_list(tail, ns)
    in
	case (s, t) of
	    (EmptySet s1, EmptySet s2) => s
	 |  (EmptySet s1, Set(l2, f2)) => s
	 |  (Set(l1, f1), EmptySet s2) => s
	 |  (Set(l1, f1), Set(l2, f2)) => if equal_set(s, t)
					  then EmptySet f1
					  else process_list(set_to_list(t), s)
    end

(* check if each element is in the second set *)
fun is_subset_of (s, t) =
    case s of
	EmptySet es => true
     |  Set([], f1) => true
     |  Set(head::[], f1) => in_set(t, head)
     |  Set(head::tail, f1) => if in_set(t, head)
			       then is_subset_of(remove_from_set(s, head), t)
			       else false

(* concatenate fstr(element) for each element *)
fun str_set (s, fstr) =
    let
	fun convertToString (lst, str) =
	    case lst of
		[] => ""
	      | head::[] => str ^ fstr(head)
	      | head::tail => convertToString(tail, str ^ fstr(head) ^ ":")
    in
	case s of
	    EmptySet _ => "{}"
	  | Set(lst, f) => convertToString(lst,"{") ^ "}"
    end
      
(* applies f to each element, add to new set *)
fun map_set (s, fcomp, f) =
    let
	fun mapList (lst, ns) =
	    case lst of
		[] => ns
	      | head::[] => insert_into_set(ns, f(head))
	      | head::tail => mapList(tail, insert_into_set(ns, f(head)))
    in
	case s of
	    EmptySet _ => EmptySet fcomp
	  | Set(lst, f) => mapList(lst, EmptySet fcomp)
    end

fun s -- v = remove_from_set(s, v)
fun s ++ v = insert_into_set(s, v)
fun s IDENTICAL t = equal_set(s, t)
fun s UNION t = union_set(s, t)
fun s INTERSECT t = intersect_set(s, t)
fun s EXCEPT t = except_set(s ,t)
fun v IN s = in_set(s, v)
fun s CONTAINS v = in_set(s, v)
fun s IS_SUBSET_OF t = is_subset_of(s, t)


(* compares elements based on possible combinations and returns expected order *)
fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    case (a, b) of
	([],[]) => EQUAL
      | ([],hd2::[]) => LESS
      | ([],hd2::tl2) => LESS
      | (hd1::[],[]) => GREATER
      | (hd1::[],hd2::[]) => (case fcomp(hd1, hd2) of
				  LESS => LESS
				| EQUAL => EQUAL
				| GREATER => GREATER)
      | (hd1::[],hd2::tl2) => (case fcomp(hd1, hd2) of
				   LESS => LESS
				 | EQUAL => LESS
				 | GREATER => GREATER)
      | (hd1::tl1,[]) => GREATER
      | (hd1::tl1,hd2::[]) => (case fcomp(hd1, hd2) of
				   LESS => LESS
				|  EQUAL => GREATER
				|  GREATER => GREATER)
      | (hd1::tl1,hd2::tl2) => case fcomp(hd1, hd2) of
				   LESS => comp_list_any(tl1, tl2, fcomp)
				 | EQUAL => comp_list_any(tl1, tl2, fcomp)
				 | GREATER => GREATER

end
end    
