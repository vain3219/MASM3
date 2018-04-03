To compile your string2.asm edit my compile.bat and copy the following lines:

ml  /coff  /c  /Zi  /Fl  %2.asm

echo.
IF NOT EXIST %2.obj echo -------------- ERRRORS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
echo.

Then paste the lines directly under these lines.  Next, change the "%2" to "%3" and save the file.

After you have saved the .bat file, using the command line go to the directory that you cloned the repository to and type
"compile masm3 string1 string2" and if there are no errors it should ask you if you wish to run the program. Press "y" 
and the program should execute in the command line.