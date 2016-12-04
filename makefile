all: parser
parser : parser.g
	antlr -gt parser.g
	dlg -ci parser.dlg scan.c
	g++ -o parser parser.c scan.c err.c -I/usr/include/pccts/ -Wno-write-strings
	./test.sh
clean :
	rm -f *.o parser.c scan.c err.c parser.dlg tokens.h mode.h parser