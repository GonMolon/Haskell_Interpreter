#!/bin/sh

PATH=$(dirname "$0")

if [ $# -lt 1 ]; then
	echo "Usage: ./test.sh programFile [inputFile]"
	exit
fi

ERROR=$( { $PATH/parser < "$1" > $PATH/programhs.txt; } 2>&1)

if [ -n "$ERROR" ]; then
	echo "Error parsing the program called \"$1\""
	echo "$ERROR" 
	exit
fi

if [ $# -gt 1 ]; then
	$PATH/interpreter < "$2"
else
	$PATH/interpreter
fi