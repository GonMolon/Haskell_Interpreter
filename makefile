all: parser interpreter
parser : reader.g
	antlr -gt reader.g
	dlg -ci parser.dlg scan.c
	g++ -o parser reader.c scan.c err.c -I/usr/include/pccts/ -Wno-write-strings
interpreter : program.hs
	ghc -o interpreter program.hs
	@chmod +x test.sh
clean :
	rm -f *.o reader.c scan.c err.c parser.dlg tokens.h mode.h parser interpreter programhs.txt program.hi