(*
Your name: Tarush Roy
Your student id: V00883469
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value


(* write your tree functions here *)

(* Part 1 - Start *)

(* Function to insert in order.
 * binding type:
 * val tree_insert_in_order : tree * int -> tree *)
fun tree_insert_in_order (t, v) =
    case t of
	emptyTree => nodeTree(v, emptyTree, emptyTree)
      | nodeTree(data, left, right) => if(v <= data)
				       then nodeTree(data, tree_insert_in_order(left, v), right)
				       else nodeTree(data, left, tree_insert_in_order(right, v))

(* "Fold" function that applies fn f to each node in tree using preorder traversal.
 * binding type:
 * val tree_fold_pre_order : (int * 'a -> 'a) -> 'a -> tree -> 'a *)
fun tree_fold_pre_order f acc t =
    case t of
	emptyTree => acc
      | nodeTree(data, left, right) => tree_fold_pre_order f (tree_fold_pre_order f (f(data, acc)) left) right

(* Finds max value in a tree.
 * binding type:
 * val tree_max : tree -> int option *)
val tree_max = fn t => SOME(tree_fold_pre_order (fn (x,y) => if(x>=y) then x else y) 0 t)

(* Function to delete node with data, t.
 * binding type:
 * val tree_delete : tree * int -> tree *)
fun tree_delete (t, v) =
    case t of
	emptyTree => raise NotFound
      | nodeTree(data, left, right) => case Int.compare(v, data) of
					   EQUAL => (case (left, right) of
							 (emptyTree, emptyTree) => emptyTree
						       | (nodeTree(data, l ,r), emptyTree) => left
						       | (emptyTree, nodeTree(data, l, r)) => right
						       | _ => nodeTree(valOf(tree_max(left)), tree_delete(left, valOf(tree_max(left))), right)
						    )
					 | LESS => nodeTree(data, tree_delete(left, v), right)
					 | GREATER => nodeTree(data, left, tree_delete(right, v))

(* Returns max height of tree.
 * binding type:
 * val tree_height : tree -> int *)
fun tree_height t =
    case t of
	emptyTree => 0
      | nodeTree(data, left, right) => 1 + Int.max(tree_height left, tree_height right)

(* Converts tree to list in pre-order.
 * binding type:
 * val tree_to_list : tree -> int list *)
val tree_to_list = fn t => tree_fold_pre_order (fn (data, acc) => acc@[data]) [] t

(* Returns tree with nodes for which f returns true.
 * binding type:
 * val tree_filter : (int -> bool) -> tree -> tree *)
fun tree_filter f t =
    case t of
	emptyTree => emptyTree
      | nodeTree(data, left, right) => if(f(data))
				       then nodeTree(data, tree_filter f left, tree_filter f right)
				       else tree_filter f (tree_delete(t, data))

(* Returns sum of nodes that are even.
 * binding type:
 * val tree_sum_even : tree -> int *)
val tree_sum_even = fn t => tree_fold_pre_order (fn (data, acc) => acc + data) 0 (tree_filter (fn x => (x mod 2) = 0) t)

(* Part 1 - END *)


(* Part 2 - Start *)

(* Return first SOME result of f on list.
 * binding type:
 * val first_answer : ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | head::tail => case f(head) of
			  NONE => first_answer f tail
			| SOME x => x

(* Returns all SOME results of f on list.
 * binding type:
 * val all_answers : ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f lst =
    case lst of
	[] => SOME []
      | head::tail => case f(head) of
			  NONE => NONE
			| SOME result => case (all_answers f tail) of
					     NONE => NONE
					   | SOME result' => SOME (result' @ result)

(* Return true if all variables in the pattern are distinct.
 * binding type:
 * val check_pattern : pattern -> bool *)
fun check_pattern p =
    false

(* I don't know what this function is supposed to do.
 * binding type:
 * val match : value * pattern -> (string * value) list option *)
fun match (value, pat) =
    NONE

(* idk
 * binding type:
 * val first_match : value -> pattern list -> (string * value) list option *)
fun first_match value pat_list =
    NONE

(* Part 2 - END *)


(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end

