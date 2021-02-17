(*
Tarush Roy
V00883469
*)

structure Babies =
struct
local
    open Csc330
in


(*
returns sum of all numbers in a list
Function taken from notes
*)
fun sum_list (num : int list) =
    if null num
    then 0
    else hd(num) + sum_list(tl num)


(*
returns a list of the second pieces of a pair in a list of pairs
Function taken from notes
*)
fun seconds (info : (int * int) list) =
    if null info
    then []
    else (#2 (hd info)) :: (seconds(tl info))


				      (*
returns an int option of the max integer in the list of tuples
(using the second piece of the tuple)
Function adapted from max function in notes
*)
fun max (nums : (int * int) list) =
    if null nums
    then NONE
    else
	let
	    fun max_nonEmpty (nums : (int * int) list) =
		if null (tl nums)
		then hd nums
		else
		    let
			val tl_ans = max_nonEmpty(tl nums)
		    in
			if #2 (hd nums) > #2 (tl_ans)
			then hd nums
			else tl_ans
		    end
	in
	    SOME (max_nonEmpty nums)
	end


				      (*
returns an int option of the minimum integer in the list of tuples
(using the second piece of the tuple)
Function adapted from max function in notes
*)
fun min (nums : (int * int) list, yrInfoList : (int * int) list) =
    if null nums
    then hd yrInfoList (* if nonZeroList is empty, all values must be zero, therefore return first pair *)
    else
	let
	    fun min_nonEmpty (nums : (int * int) list) =
		if null (tl nums)
		then hd nums
		else
		    let
			val tl_ans = min_nonEmpty(tl nums)
		    in
			if #2 (hd nums) <= #2 (tl_ans)
			then hd nums
			else tl_ans
		    end
	in
	    min_nonEmpty(nums)
	end


(*
counts number of years where baby name appears
*)
fun yearCount (yrInfoList : (int * int) list, count : int) =
    let
	val pair = hd yrInfoList;
	val num = #2 pair;
    in
	if null (tl yrInfoList)
	then if num = 0 then count else count+1
	else
	    if num = 0
	    then yearCount(tl yrInfoList, count)
	    else yearCount(tl yrInfoList, count+1)
    end


(*
returns a pair containing the last year and its value
*)
fun lastYrInfo (yrInfoList : (int * int) list) =
    let
	val pair = hd yrInfoList;
	val year = #1 pair;
	val num = #2 pair;
    in
	if null (tl yrInfoList)
	then (year, num)
	else lastYrInfo(tl yrInfoList)
    end


(*
returns a pair containing the first year with data and its value
*)
fun firstYrInfo (yrInfoList : (int * int) list, firstPair : int * int) =
    let
	val pair = hd yrInfoList;
	val year = #1 pair;
	val num = #2 pair;
    in
	if null (tl yrInfoList)
	then if num <> 0 then pair else firstPair
	else
	    if num <> 0
	    then (year, num)
	    else firstYrInfo(tl yrInfoList, firstPair)
    end


(*
returns a pair containing the last year with data and its value
*)
fun lastYrWithData (yrInfoList : (int * int) list, storedPair : (int * int)) =
    let
	val pair = hd yrInfoList;
	val year = #1 pair;
	val num = #2 pair;
    in
	if null (tl yrInfoList)
	then if num <> 0 then pair else if #1 storedPair = 0 then pair else storedPair
	else
	    if num <> 0
	    then lastYrWithData(tl yrInfoList, pair)
	    else lastYrWithData(tl yrInfoList, storedPair)
    end


(*
removes zeros from list of years and values
*)
fun removeZeros (yrInfoList : (int * int) list) =
    let
	val pair = hd yrInfoList;
	val year = #1 pair;
	val num = #2 pair;
    in
	if null (tl yrInfoList)
	then if num <> 0 then pair::[] else []
	else
	    if num <> 0
	    then pair :: removeZeros(tl yrInfoList)
	    else removeZeros(tl yrInfoList)
    end


(*
prints all information required based on baby info
*)
fun printBabyInfo (babyInfo : string * int * (int * int) list) =
    let
	val babyName = #1 babyInfo;
	val total = #2 babyInfo;
	val numYears = yearCount(#3 babyInfo, 0);
	val lYrPair = lastYrInfo(#3 babyInfo);
	val fYrPair = firstYrInfo(#3 babyInfo, hd (#3 babyInfo));
	val lYrDataPair = lastYrWithData(#3 babyInfo, (0, 0));
	val nonZeroList = removeZeros(#3 babyInfo);
	val minPair = min(nonZeroList, #3 babyInfo);
	val maxOpPair = max(#3 babyInfo);
	val maxPair = valOf(maxOpPair);
	val avg = int_to_real(total) / int_to_real(length(#3 babyInfo));

	val _ = print(babyName ^ "\n");
	val _ = print(" Total: " ^ int_to_string(total) ^ "\n");
	val _ = print(" Years: " ^ int_to_string(numYears) ^ "\n");
	val _ = print(" " ^ int_to_string(#1 lYrPair) ^ ": " ^ int_to_string(#2 lYrPair) ^ "\n");
	val _ = print(" First: " ^ int_to_string(#1 fYrPair) ^ " " ^ int_to_string(#2 fYrPair) ^ "\n");
	val _ = print(" Last: " ^ int_to_string(#1 lYrDataPair) ^ " " ^ int_to_string(#2 lYrDataPair) ^ "\n");
	val _ = print(" Min: " ^ int_to_string(#1 minPair) ^ " " ^ int_to_string(#2 minPair) ^ "\n");
	val _ = print(" Max: " ^ int_to_string(#1 maxPair) ^ " " ^ int_to_string(#2 maxPair) ^ "\n");
	val _ = print(" Avg: " ^ real_to_string(avg) ^ "\n");
    in
	()
    end


(*
given baby name, search processed list of babies if name exists
*)
fun search (name : string, listOfBabies : (string * int * (int * int) list) list) =
    let
	val baby = hd listOfBabies;
    in
	if null (tl listOfBabies)
	then NONE
	else
	    if name = #1 baby
	    then SOME (baby)
	    else search(name, tl listOfBabies)
    end
	

(*
function to check and print appropriate message if baby name is not found in the list
*)
fun checkAndPrint (name : string, baby : (string * int * (int * int) list) option) =
    if isSome(baby)
    then printBabyInfo(valOf(baby))
    else print(name ^ "\n" ^ "Baby name [" ^ name ^ "] was not found\n")


(*
function that calls search and check functions
*)
fun searchAndPrint (inputNames : string list, listOfBabies : (string * int * (int * int) list) list) =
    if null inputNames
    then ()
    else
	let
	    val name = hd inputNames;
	    val baby = search(name, listOfBabies);
	    val _ = checkAndPrint(name, baby);
	in
	    if null (tl inputNames)
	    then ()
	    else searchAndPrint(tl inputNames, listOfBabies)
	end


(*
function: process_entries
function type: (int * int) list
*)
fun process_entries (entries : string list, count : int, yearSt : int) =
    let
	val num = valOf(fromString(hd entries));
    in
	if null(tl entries)
	then []
	else (yearSt + count, num) :: process_entries(tl entries, count+1, yearSt)
    end
	

(*
function type: (string * int * (int * int) list)
*)
fun process_record (line : string list, yearSt : int) =
    let
	val babyName = hd line;
	val yearInfo = process_entries(tl line, 0, yearSt);
	val total = sum_list(seconds(yearInfo));
    in
	(babyName, total, yearInfo)
    end
	

(*
function type: (string * int * (int * int) list) list
*)
fun process_records (lines : string list, yearSt : int) =
    let
	val line = split_at(hd lines, #",");
	val record = process_record(line, yearSt);
    in
	if null lines
	then []
	else
	    if null(tl lines)
	    then []
	    else record :: process_records(tl lines, yearSt)
    end

	
(*
when lines is non-empty, this function will run
*)
fun babies_program_nonEmpty (lines : string list, yearSt : string) =
    let
	(*
        using one line, calculate number of entries
        number of tokens -2 because
        -1 for baby name and -1 for total
        *)
	val line = split_at(hd lines, #",");
	val numEntries = (length (line)) - 2;
	val _ = print ("Each baby has " ^ int_to_string(numEntries) ^ " entries"^ dot ^"\n");

	(*
        process records
        get list of tuples containing info for each baby
        lines type: string list
        listOfBabies type: (string * int * (int * int) list) list
	*)
	val listOfBabies = process_records(lines, valOf(fromString(yearSt)));

	(*
        read from std in, save as input string
	*)
	val input = read_stdin();

	(*
        parse input and search names
        print relevant information
	*)
	val inputNames = split_at(input, #"\n");
	val _ = searchAndPrint(inputNames, listOfBabies);
    in
	()
    end
	
	
(*
when lines is empty, this function runs to handle empty case
*)
fun babies_program_empty (lines : string list) =
    let
	val _ = print("\n");
	
	(*
        read from std in, save as input string
        *)
	val input = read_stdin();

	(*
        parse input and search names
        print relevant information
	*)
	val inputNames = split_at(input, #"\n");
	val _ = searchAndPrint(inputNames, [("",0,[(0,0)])]);
    in
	()
    end
	

(*
function to implement for assignment
*)
fun babies_program (fileName, yearSt) =
    let
	(* read file into string, fileContent *)
	val fileContent = read_file(fileName);

	(*
	split file into lines
	calculate and print number of lines
	*)
	val lines = split_at(fileContent, #"\n");
	val numBabies = length lines;
	val _ = print ("Read " ^ int_to_string(numBabies) ^ " babies" ^ dot ^ " ");

	(* print starting year *)
	val _ = print ("Starting year " ^ yearSt ^ dot ^ " ");
    in
	if null lines
	then babies_program_empty(lines)
	else babies_program_nonEmpty(lines, yearSt)
    end
	

(*
do not modify below this point
*)
        
fun main (prog_name, args) =
    let
      val (_, fileName, offsetSt) = parse_command_line args
      val _ = babies_program(fileName, offsetSt)
    in
      exit()
    end

end

end
    
