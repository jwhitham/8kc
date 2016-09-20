
main: main.adb Makefile
	gnatmake -g -O0 -gnata -gnat12 main.adb

test: main code.txt support.o Makefile
	./main < code.txt > a.s
	nasm -f elf32 a.s
	gcc -o a.exe a.o support.o -m32
	./a.exe > a.txt
	diff -q a.txt a.ref

support.o: support.c Makefile
	gcc -c support.c -O2 -m32

clean:
	rm -f main *.ali *.o a.txt a.exe
