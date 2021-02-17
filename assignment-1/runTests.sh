#!/bin/bash

echo Checking for dots in babies.sml...
python3 checkForDots.py

printf "\nTesting babies.sml against test cases...\n"

NUMTESTS=5

for (( i=0; i<$NUMTESTS; i++ )); do
	inpcmd='test-0'$i
	expfile='test-0'$i'.expected'
	outfile='test-0'$i'.result'
	make $inpcmd > tests/$outfile
	diff tests/$outfile tests/$expfile > temp.txt
	if [ ! -s temp.txt ]
	then
		echo $inpcmd passed.
	else
		echo $inpcmd failed.
	fi
	rm temp.txt
done

