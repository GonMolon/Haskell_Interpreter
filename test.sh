#!/bin/sh

rm -f programhs.txt

PATH=$(dirname "$0")

if [ $# -lt 1 ]; then
	echo "Usage: ./test.sh programFile [inputFile]"
	exit
fi

$PATH/parser < "$1" > $PATH/programhs.txt

if [ -s $PATH/programhs.txt ]; then
	if [ $# -gt 1 ]; then
		$PATH/interpreter < "$2"
	else
		$PATH/interpreter
	fi
fi