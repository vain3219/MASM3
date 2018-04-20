
	.486

	;Includes
	include ..\..\Irvine\Irvine32.inc


getstring	PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
putstring	PROTO Near32 stdcall, lpStringToPrint:dword
ascint32 	PROTO Near32 stdcall, lpStringOfNumericChars:dword
intasc32	PROTO Near32 stdcall, lpStringToHold:dword, dVal:dword
memoryallocBailey  PROTO Near32 stdcall, dNumBytes:dword 
ExitProcess PROTO, dwExitCode:dword

.data
	strEnter1 byte 10,9,"enter a string: ",0
	strEnter2 byte 10,9,"enter a string: ",0
	strString1 byte 32 dup(0)
	strString2 byte 32 dup(0)
	lowerPrompt byte 10,9,"Enter the number of the string you wish to convert: ",0
	strIndexPrompt byte 10,9,"Enter the character you are looking for: ",0
	strIndexError byte 10,9,"Sorry, that character does not exist in String 1",0
	invalidPrompt1 byte 10,9,"Sorry, string ",0
	invalidPrompt2 byte " does not exist. Please try again.",0
	userInput byte 2 dup(?)
	indexOF2input byte 3 dup(0)
	indexOf2Prompt byte 10,9,"Enter the index you wish to start the search from (0 to 31): ",0
	strIndexError2 byte 10,9,"Sorry, the character you have chosen is not present after the specified index",0
	toReplacePrompt byte 10,9,"Please pick a character to replace: ",0
	ReplaceWith byte 10,9,"Please pick a replacement: ",0
	strChartoReplace byte 2 dup(0)
	strReplace byte 2 dup (0)
	memError byte 10,9,"sorry, there is not enough memory available.",0
.code

;-------------------------------------------------------------------------------------------------
String_toLowerCase PROC
;
;		prompts user for a string choice, and then changes their choice to lowercase
;----------------------------------------------------------------------------------------------------

	push EBP											;save ebp
	push EDI
	push esi											;save esi
	push ebx											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	mov eax,0
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input prompt
	mov AL, userInput
	.IF AL == 31h	
		mov EBX, [EBP + 20]								;move offset address of string1 into EBX
		mov EDI, [EBP + 20]
		jmp _lowerconvert								;jump to the conversion
	.ELSEIF AL == 32h
		mov EBX, [EBP + 24]								;move offset address of string2 into EBX
		mov EDI, [EBP + 24]
		jmp _lowerconvert								;jump to the conversion
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF
	
_lowerconvert:

	cmp byte ptr[EBX+ESI],0 							;reached the end of he string if character == null
	JE _end												;jump to the end once the end of the string is reached
	cmp byte ptr[EBX+ESI],5Ah 							;don't convert characters that aren't letters
	JG _next											;jump to next
	cmp byte ptr[EBX+ESI],41h							;don't convert anything that isn't a uppercase letter
	JL _next											;jump if less to next
	add byte ptr[EBX+ESI],20h							;converts uppercase to lowercase
_next:

	inc ESI												;moves to next byte location 
	jmp _lowerconvert									;jump to beginning of loop
	
_end:
	mov EAX, EDI
	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI
	pop EBP												;restore ebp
	ret													;return 
	
String_toLowerCase ENDP

;----------------------------------------------------------------------------------------------------
String_toUpperCase PROC
;
;		prompts user for a string choice, and then converts that string to all uppercase
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push EDI											;save eax
	push ESI											;save esi
	push EBX											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	mov eax,0
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input prompt
	mov AL, userInput
	.IF AL == 31h	
		mov EBX, [EBP + 20]								;move offset address of string1 into EBX
		mov EDI, [EBP + 20]
		jmp _upperconvert								;jump to the conversion
	.ELSEIF AL == 32h
		mov EBX, [EBP + 24]								;move offset address of string2 into EBX
		mov EDI, [EBP + 24]
		jmp _upperconvert								;jump to the conversion
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF
	
_upperconvert:

	cmp byte ptr[EBX+ESI],0 							;reached the end of he string if character == null
	JE _end												;jump to the end once the end of the string is reached
	cmp byte ptr[EBX+ESI],7Ah 							;don't convert characters that aren't letters
	JG _next											;jump to next
	cmp byte ptr[EBX+ESI],61h							;don't convert anything that isn't a uppercase letter
	JL _next											;jump if less to next
	AND byte ptr[EBX+ESI], 0DFh							;converts lowercase to uppercase
	
_next:

	inc ESI												;moves to next byte location 
	jmp _upperconvert									;jump to beginning of loop
	
_end:
	mov EAX, EDI
	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI												;restore edi
	pop EBP												;restore ebp
	ret													;return 
	
String_toUpperCase ENDP

;----------------------------------------------------------------------------------------------------
String_indexOf_1 PROC
;
;		prompts user to choose a character, finds first instance of said character and 
;		returns the index of the first instance.
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	invoke putstring, ADDR strIndexPrompt				;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	mov EBX, [EBP + 20]									;move offset address of string1 into EBX

_search:

														
														
	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _endstring									;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		Jmp _next										;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring:

	invoke putstring, addr strIndexError				;display error if character not in string
	jmp _end											;jump to the end

_next:
	
	mov EAX, ESI										;stores index in EAX
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EBP												;restore ebp
	ret													;return 

String_indexOf_1 ENDP

;----------------------------------------------------------------------------------------------------
String_indexOf_2 PROC
;
;		prompts user to choose a character, then an index. finds first instance of said character 
;		after the chose index and returns the index of the first occurrence afterwards
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clear ECX
	mov EAX, 0											;clear EAX
	invoke putstring, ADDR strIndexPrompt				;displays input prompt for character
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	invoke putstring, ADDR indexOf2Prompt				;displays input prompt for the index
	invoke getstring, ADDR indexOF2input,2				;gets user input
	invoke ascint32, ADDR indexOF2input					;converts user input to an integer for indexing
	mov EBX, [EBP + 20]									;move offset address of string1 into EBX
	mov ESI, EAX										;move specified index into ESI 
	mov EAX, 0											;clear EAX
	
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _endstring									;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		Jmp _next										;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring:

	invoke putstring, addr strIndexError2				;display error if character not in string after specified index
	jmp _end											;jump to the end
	
_next:

	mov EAX, ESI

_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EBP												;restore ebp
	ret													;return
	
String_indexOf_2 ENDP

;----------------------------------------------------------------------------------------------------
String_indexOf_3 PROC
;
;		finds the first instance of string 2 within string 1 and returns the index
;		
;----------------------------------------------------------------------------------------------------	
	push EBP											;save ebp
	push EDX											;save EDX
	push EDI											;save EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov EDI,0											;clears EDI for indexing into string
	mov ECX,0											;clears ECX
	mov EAX,0											;clears eax
	mov EBX, [EBP + 28]									;move offset address of string1 into EBX
	mov ECX, [EBP + 32]									;move offset address of string2 into ECX
	

_search:
	
	mov AL, byte ptr[ECX+EDI]							;moves each byte into AL for comparison
	mov AH, byte ptr[EBX+ESI]							;moves each byte into AH for comparison
	.IF AL == 0											;reached the end of string2 if character == null
		jmp _endstring2									;jump to _endstring once the end of the string is reached
	.ELSEIF AH == 0										;checks to see if end of string 1
		Jmp _endstring1
	.ELSEIF AL == AH
		mov EDX, ESI
		inc EDI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring1:

	invoke putstring, addr strIndexError
	jmp _end											;jump to the end

_endstring2:

	mov EAX, 0
	inc EDX
	sub EDX, EDI
	mov AL, DL
	
_end:
	mov eax, esi
	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI												;restore EDI
	pop EDX												;restore EDX
	pop EBP												;restore ebp
	ret													;return 

String_indexOf_3 ENDP

;----------------------------------------------------------------------------------------------------
String_lastIndexOf_1 PROC
;
;	prompts user to enter a character, and then finds the last occurrence of the chosen character
;   and returns the index.
;		
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	invoke putstring, ADDR strIndexPrompt				;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	mov EBX, [EBP + 24]									;move offset address of string1 into EBX

_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _next									    ;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov EDI, ESI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	

_next:
	
	mov EAX, EDI										;stores index in EAX
	jmp _end
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	Pop EDI
	pop EBP												;restore ebp
	ret			

String_lastIndexOf_1 ENDP

;----------------------------------------------------------------------------------------------------
String_lastIndexOf_2 PROC
;
;	prompts user to enter a character, then an index to start the search from, and then finds the 
;   last occurrence of the  chosen character and returns the index.
;		
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clear ECX
	mov EAX, 0											;clear EAX
	invoke putstring, ADDR strIndexPrompt				;displays input prompt for character
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	invoke putstring, ADDR indexOf2Prompt				;displays input prompt for the index
	invoke getstring, ADDR indexOF2input,2				;gets user input
	invoke ascint32, ADDR indexOF2input					;converts user input to an integer for indexing
	mov EBX, [EBP + 24]									;move offset address of string1 into EBX
	mov ESI, EAX										;move specified index into ESI 
	mov EAX, 0											;clear EAX
	
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _next									    ;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov EDI, ESI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	

_next:
	
	mov EAX, EDI										;stores index in EAX
	jmp _end

_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI
	pop EBP												;restore ebp
	ret													;return

String_lastIndexOf_2 ENDP

;----------------------------------------------------------------------------------------------------
String_lastIndexOf_3 PROC
;
;	finds the last occurrence of string 2 within string 1 and returns the index
;		
;----------------------------------------------------------------------------------------------------

	push EBP											;save ebp
	push EDX											;save EDX
	push EDI											;save EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov EDI,0											;clears EDI for indexing into string
	mov ECX,0											;clears ECX
	mov EAX,0											;clears eax
	mov EBX, [EBP + 28]									;move offset address of string1 into EBX
	mov ECX, [EBP + 32]									;move offset address of string2 into ECX
	

_search:
	
	mov AL, byte ptr[ECX+EDI]							;moves each byte into AL for comparison
	mov AH, byte ptr[EBX+ESI]							;moves each byte into AH for comparison
	.IF AL == 0											;reached the end of string2 if character == null
		jmp _endstring2									;jump to _endstring once the end of the string is reached
	.ELSEIF AH == 0										;checks to see if end of string 1
		Jmp _endstring1
	.ELSEIF AL == AH
		mov EDX, ESI
		inc EDI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring2:
	mov EDI, 0
	inc ESI
	jmp _search

_endstring1:

	mov EAX, 0
	dec EDX
	sub EDX, EDI
	mov AL, DL
	
_end:
	mov eax, esi
	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI												;restore EDI
	pop EDX												;restore EDX
	pop EBP												;restore ebp
	ret													;return 

String_lastIndexOf_3 ENDP

;----------------------------------------------------------------------------------------------------
String_replace PROC
;
;	prompts user to choose a string, then choose a character they wish to replace, and then choose
;	the character they would like to replace it with.
;		
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push EDI											;save EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov AL, userInput									;stores userInput in AL for comparison
	.IF AL == 31h	
		mov EBX, [EBP + 24]								;move offset address of string1 into EBX
		mov EDI, [EBP + 24]
	.ELSEIF AL == 32h
		mov EBX, [EBP + 28]								;move offset address of string2 into EBX
		mov EDI, [EBP + 28]
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF

	invoke putstring, ADDR toReplacePrompt
	invoke getstring, ADDR strChartoReplace,1
	invoke putstring, ADDR ReplaceWith
	invoke getstring, ADDR strReplace,1
	mov CL, strChartoReplace
	mov CH, strReplace
	
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _end										;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov byte ptr[EBX+ESI],CH						;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_end:
	mov EAX, EDI
	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI
	pop EBP												;restore ebp
	ret													;return 

String_replace ENDP

;----------------------------------------------------------------------------------------------------
String_concat PROC
;
;	concatenates string 2 to the end of string 1
;		
;----------------------------------------------------------------------------------------------------
	push EBP											;save ebp
	push EDI											;save EDI
	push EDX											;save EDX
	push esi											;save esi
	push ebx											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	mov EAX,0											;Clear EAX
	mov edi,0											;clear EDI
	invoke memoryallocBailey, 64						;allocate memory for new string
	.IF EAX == 0										
		jmp _error
	.ELSE
		mov EDX, EAX
	.ENDIF
	mov ebx, [EBP+24]									;move offset of string 1 into ebx
	mov EAX,0
	
_start:
	mov AH, byte ptr [EDX+EDI]							;index into new memory
	mov AL, byte ptr [EBX+ESI]							;index into string1
	.IF AL != 0											;keep going if not at end of the string
		mov AH, AL										;copy byte from str1 into new location
		inc esi											;increment index
		inc edi											;increment index
	.ELSE
		jmp _next
	.ENDIF
	jmp _start
	
_next:
	mov esi,0
	mov ebx, [EBP+28]									;offset string 2
	
_loop:
	mov AH, byte ptr [EDX+EDI]							;index into new memory
	mov AL, byte ptr [EBX+ESI]							;index into string2
	.IF AL != 0											;this loop is the same for string 2
		mov AH, AL
		inc esi
		inc edi
		jmp _loop
	.ENDIF
	
	mov EAX, EDX
	jmp _end
_error:
	invoke putstring, addr memError 
_end:
	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDX
	pop EDI
	pop EBP												;restore ebp
	ret													;return 

String_concat ENDP



END