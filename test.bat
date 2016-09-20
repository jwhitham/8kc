c:\gnat\2016\bin\gnatmake -g -O0 -gnata main.adb -o 8kc.exe
if %errorlevel% neq 0 exit /b
8kc.exe < code.txt > a.s
if %errorlevel% neq 0 exit /b
c:\jack\nasm-2.12.02\nasm.exe --prefix _ -f win32 a.s
if %errorlevel% neq 0 exit /b
c:\gnat\2016\bin\gcc -o a.exe support.c a.obj
if %errorlevel% neq 0 exit /b
a.exe > a.txt
if %errorlevel% neq 0 exit /b
