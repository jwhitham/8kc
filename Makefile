
SRC=backend.adb backend.ads grammar.adb grammar.ads \
lex.adb lex.ads main.adb types.ads

8kc: Makefile $(SRC)
	gnatmake -g -O0 -gnata -gnat12 main.adb -o 8kc

test: 8kc code.txt support.o Makefile
	./8kc < code.txt > a.s
	nasm -f elf64 a.s
	gcc -o a.exe a.o support.o -m64
	./a.exe > a.txt
	diff -q a.txt a.ref

support.o: support.c Makefile
	gcc -c support.c -O2 -m64

clean:
	rm -f 8kc *.ali *.o a.txt a.exe
