set CURRDIR=%~dp0
set HOME=%CURRDIR%\..
call %HOME%\miniconda3\Scripts\activate.bat
..\Programs\emacs\bin\runemacs.exe
