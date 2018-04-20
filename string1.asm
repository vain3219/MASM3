;----------------------------------------------------------------------------------------------------
;	FILE NAME : string1.asm
;----------------------------------------------------------------------------------------------------
;
;		Program Name	:	MASM3
;		Programmer		:	Cody Thompson 
;		Class			:	CS 3B || Asm Lang
;		Date			:	4/02/2018
;		Purpose			:
;		This file contains the definitions for the external sub routines that will be called by the driver
;	MASM3.asm
;----------------------------------------------------------------------------------------------------

	.486

	;Includes
	include ..\..\Irvine\Irvine32.inc

	;Prototypes
	getstring			PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
	putstring			PROTO Near32 stdcall, lpStringToPrint:dword
	memoryallocBailey	PROTO NEAR32 stdcall, dSize:dword
	ascint32			PROTO Near32 stdcall, lpStringToConvert:dword  
	String_length		PROTO intStrAddr:dword
	ExitProcess			PROTO, dwExitCode:dword

	;Constants


	;Data segment
	.data
strSetString		 		BYTE 	"Please input a string: ", 0
	
strNwLn						BYTE 	0Ah, 0	
	;Code segment
	.code						

;----------------------------------------------------------------------------------------------------
setString proc PUBLIC
;
;		Sets the value of String1.
;
;	Receives the offset address of strString1 from masm3.asm.
;	Returns nothing
;----------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP + 8]
	PUSH EBP
	MOV EBP, ESP
	
	CALL Clrscr										;clear the screen
	
	INVOKE putString, addr strSetString				;output strSetString to the console
	INVOKE getString, intStrAddr, 32				;get input from the console and store it in memory labeled intStr1Addr
	
	POP EBP
	RET												;return
setString ENDP									;end of setString1

;----------------------------------------------------------------------------------------------------
String_equals proc PUBLIC
;
;		This sub routine will compare strString1 and strString2 for an exact match.  This is a case
;	and length sensitive comparison.  If a match is found 01 will be returned to the AL register,
;	otherwise 00 will be returned if the comparison is false.
;
;	Receives the offset address of strString1, strString2, and lengths of both strString1 & 2 from the stack 
;	Returns results to the AL register
;----------------------------------------------------------------------------------------------------
intStr1Addr EQU [EBP+8]
intStr2Addr EQU [EBP+12]

	PUSH EBP
	MOV EBP, ESP
	
	MOV EDI, intStr1Addr							;move intStr1Addr into EDI for indirect addressing
	MOV EDX, intStr2Addr							;move intStr2Addr into EDX for indirect addressing 
	INVOKE String_length, EDI						;get length of intStr1Addr
	MOV ECX, EAX									;set ECX equal to EAX
	
	INVOKE String_length, EDX						;get length of intStr2Addr
	
	CMP ECX, EAX									;compare lengths of string1 and string2
	JNE NOTEQL										;if the lengths aren't equal jump to NOTEQL
	
	MOV EDI, intStr1Addr							;move intStr1Addr into EDI for indirect addressing
	MOV EDX, intStr2Addr							;move intStr2Addr into EDX for indirect addressing 
	
L1:
	MOV BL, [EDI]									;move the element at the address in EDI into BL
	CMP BL, [EDX]									;compare BL to the element at the address in EDX
	JNE NOTEQL										;if BL and [EDX] are not equal then jump to NOTEQL
	INC EDI											;go to the next element in EDI
	INC EDX											;go to the next element in EDX
	LOOP L1											;loop to L1
	
	MOV AL, 1										;TRUE CONDITION -- move 1 into AL
	JMP RETURN										;jump to RETURN
	
NOTEQL:
	MOV AL, 0										;FALSE CONDITION -- move 0 into AL
	
RETURN:
	POP EBP
	RET
String_equals ENDP


;----------------------------------------------------------------------------------------------------
String_equalsIgnoreCase proc PUBLIC
;
;		This sub routine will compare strString1 and strString2 for an exact match.  This is not case
;	sensitive but is length sensitive.  If a match is found 01 will be returned to the AL register,
;	otherwise 00 will be returned if the comparison is false.
;
;	Receives the offset address of strString1, strString2, and lengths of both strString1 & 2 from the stack 
;	Returns results to the AL register
;----------------------------------------------------------------------------------------------------
intStr1Addr EQU [EBP+8]
intStr2Addr EQU [EBP+12]

	PUSH EBP										;save EBP
	MOV EBP, ESP									;set EBP=ESP
	
	MOV EDI, intStr1Addr							;move intStr1Addr into EDI for indirect addressing
	INVOKE String_length, EDI

	MOV ECX, EAX        							;move intStr1Length into ECX for LOOP instruction
	MOV EDX, intStr2Addr							;move intStr2Addr into EDX for indirect addressing 
	
	INVOKE String_length, EDX
	
	CMP ECX, EAX									;compare lengths of string1 and string2
	JNE NOTEQL										;if the lengths aren't equal jump to NOTEQL
	
	MOV EDI, intStr1Addr							;move intStr1Addr into EDI for indirect addressing
	MOV EDX, intStr2Addr							;move intStr2Addr into EDX for indirect addressing 
	
L1:
	MOV EAX, 1										;move 1 into EAX to keep track of how many times each index if compared (2 max)
	MOV BL, [EDI]									;move the element at the address in EDI into BL
L2:	
	CMP BL, [EDX]									;compare BL to the element at the address in EDX
	JE MATCH										;jump to MATCH if BL == [EDX]
	
	CMP EAX, 0										;compare EAX to zero, checking if the index has been compared once already
	JE NOTEQL										;jump to NOTEQL if EAX == 0
	
	CMP BL, 41h										;ensure that BL isn't less than 41h (special character)
	JL NOTEQL										;jump to NOTEQL if bl is less than 41h
	CMP BL, 5Ah										;ensure that BL is in the range of 41h - 5Ah
	JL UPPER										;if BL is less than 5A then it is within the range, jump to UPPER

	CMP BL, 61h										;ensure that BL isn't less than 61h (special character)
	JL NOTEQL										;jump to NOTEQL if BL is less than 61h
	CMP BL, 7Ah										;ensure that BL is in the range of 61h - 7Ah
	JL LOWER										;if BL is less than 7A then it is within the range, jump to LOWER
	
	JNE NOTEQL										;if BL and [EDX] are not equal then jump to NOTEQL
MATCH:
	INC EDI											;go to the next element in EDI
	INC EDX											;go to the next element in EDX
	LOOP L1											;loop to L1
	
	MOV AL, 1										;TRUE CONDITION -- move 1 into AL
	JMP RETURN										;jump to RETURN
	
UPPER:
	DEC EAX											;decrement EAX
	ADD BL, 20h										;add 20h to BL making it the lowercase complement
	JMP L2											;jump to L2
	
LOWER:
	DEC EAX											;decrement EAX 
	SUB BL, 20h										;subtract 20h from BL making it the uppercase complement
	JMP L2											;jump to L2
	
NOTEQL:
	MOV AL, 0										;FALSE CONDITION -- move 0 into AL
	
RETURN:
	POP EBP
	RET
String_equalsIgnoreCase ENDP


;----------------------------------------------------------------------------------------------------
String_copy proc PUBLIC
;
;		This sub routine will copy the contents of strString1 into newly allocated memory.  Memory will
;	be allocated using Dr. Baileys memoryallocBailey procedure.  The address of the newly allocated 
;	string with the copied contents will be returned to the EAX register.
;
;	Receives the address of strString1 and its length from the stack
;	Returns the address of the new string to the EAX register
;----------------------------------------------------------------------------------------------------
intStr1Addr EQU [EBP+8]	

	PUSH EBP										;save EBP
	MOV EBP, ESP									;set EBP=ESP
	
	MOV ESI, intStr1Addr							;move the offset address of string1 into ESI
	INVOKE String_length, ESI						;get the length of ESI
	MOV ECX, EAX		 							;move the length of string1 into ECX
	INC ECX											;increment ECX to account for the null terminator
	
	INVOKE memoryallocBailey, ECX					;allocate a memory of size string1Length +1
	MOV EDI, EAX									;move the newly allocated address into EDI
	DEC ECX											;decrement ECX by one
	
L1:
	MOV BL, [ESI]									;move the nth index of [ESI] into BL
	MOV [EDI], BL									;move BL into nth index of [EDI]
	INC EDI											;increment EDI by one
	INC ESI											;increment ESI by one 
	LOOP L1											;LOOP to L1
	
	MOV BL, [ESI + 1]								;move the null terminator into BL
	MOV [EDI + 1], BL								;move the null terminator into the last index of [EDI]
	
	POP EBP
	RET			
String_copy ENDP


;----------------------------------------------------------------------------------------------------
String_substring_1 proc PUBLIC
;
;		This sub routine will create a newly allocated comprised of a sub string from the string of the 
;	address provided on the stack.  A prompt will be displayed to the console asking the user for a 
;	beginning and ending index.  The address of the newly allocated string will be return to the EAX
;	register.
;
;	Receives the address of a string and its length
;	Returns the address of the new string to the EAX register
;----------------------------------------------------------------------------------------------------
intStrAddr  EQU [EBP+8]
intStartInd  EQU [EBP+12]
intEndInd  EQU [EBP+16]
	
	PUSH EBP
	
	MOV EBP, ESP
	
	MOV ESI, intStrAddr								;move the offset address of intStrAddr into ESI
	ADD ESI, intStartInd							;move the to specified starting address
	MOV ECX, intEndInd								;move the ending index number into ECX
	SUB ECX, intStartInd							;subtract tarting index from ending index to get 
	INC ECX
	
	INVOKE memoryallocBailey, ECX					;allocate memory of size (end - start) + 1
	MOV EDI, EAX									;move the address of the newly allocated string into the ESI register
	
L1:
	MOV BL, [ESI]									;move the nth element of [ESI] into BL
	MOV [EDI], BL									;move BL into the nth element of [EDI]
	INC EDI											;increment EDI by one
	INC ESI											;increment ESI by one
	LOOP L1											;LOOP to L1
	
	MOV BL, [ESI + 1]								;move the null terminator into BL
	MOV [EDI + 1], BL								;move BL into [EDI + 1]
	JMP RETURN										;jump to RETURN
	
RETURN:
	POP EBP
	RET
String_substring_1 ENDP


;----------------------------------------------------------------------------------------------------
String_substring_2 proc PUBLIC
;
;		This sub routine will create a newly allocated comprised of a sub string from the string of the 
;	address provided on the stack.  A prompt will be displayed to the console asking the user for a 
;	beginning index.  The address of the newly allocated string will be return to the EAX
;	register.
;
;	Receives the address of a string and its length
;	Returns the address of the new string to the EAX register
;----------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP+8]
intStartInd EQU [EBP+12]

	PUSH EBP										;save EBP on the stack	
	MOV EBP, ESP									;set EBP = ESP
	
	MOV ESI, intStrAddr								;move the offset address of intStrAddr into ESI
	ADD ESI, intStartInd							;move the to specified starting address
	
	INVOKE String_length, intStrAddr				;get length of [intStrAddr]
	MOV ECX, EAX									;move the length into ECX for LOOP
	
	INVOKE memoryallocBailey, ECX					;allocate memory of size (end - start) + 1
	MOV EDI, EAX									;move the address of the newly allocated string into the ESI register
	
L1:
	MOV BL, [ESI]									;move the nth element of [ESI] into BL
	MOV [EDI], BL									;move BL into the nth element of [EDI]
	INC EDI											;increment EDI by one
	INC ESI											;increment ESI by one
	LOOP L1											;LOOP to L1
	
	MOV BL, [ESI + 1]								;move the null terminator into BL
	MOV [EDI + 1], BL								;move BL into [EDI + 1]
	JMP RETURN										;jump to RETURN
	
RETURN:
	POP EBP
	RET
String_substring_2 ENDP


;----------------------------------------------------------------------------------------------------
String_charat proc PUBLIC
;
;		This sub routine will return the character at the specified index to the AL register.  if the 
;	index is an impossible location a 0 will be returned to the AL register.
;
;	Receives a string address and the length of that string from the stack
;	Returns a character to the AL register
;----------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP+8]
intIndex EQU [EBP+12]

	PUSH EBP
	MOV EBP, ESP
	
	MOV EAX, intIndex								;move index into EAX
	CMP EAX, 1										;compare result to 1
	JL OUTOFBOUNDS									;jump to OUTOFBOUNDS if the input value is >1
	INVOKE String_length, intStrAddr				;get length of [intStrAddr]	
	CMP intIndex, EAX								;compare result to stringLrngth
	JG OUTOFBOUNDS									;jump to OUTOFBOUNDS if the input value > length

	MOV EAX, intIndex
	DEC EAX											;decrement EAX
	MOV EDI, intStrAddr								;move the offset address of the string into EDI
	ADD EDI, EAX									;move to the address of the specified index
	MOV AL, [EDI]									;move the contents at the index into AL
	JMP RETURN
	
OUTOFBOUNDS:
	MOV AL, 0										;move 0 into AL
	
RETURN:	
	POP EBP
	RET
String_charat ENDP


;-----------------------------------------------------------------------------------------------------------
String_startsWith_1 proc PUBLIC
;
;		This sub routine checks if string1 starts with string2 at the specified starting position.
;	If the comparison is true a 1 will be returned into the AL register, otherwise a zero will be returned.
;
;	Receives the addresses of string1, string2, and an integer
;	Returns a 1 or 0 to the AL register
;-----------------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP+8]
intStr2Addr EQU [EBP+12]
intPos EQU [EBP+16]

	PUSH EBP										;save EBP
	MOV EBP, ESP									;set ebp = esp
	
	MOV EDI, intStrAddr								;move string1 address into EDI
	MOV ESI, intStr2Addr							;move string2 address into ESI
	MOV EBX, intPos 								;move intPos into EBX
	ADD EDI, ebx									;add the offset of the position to string1
	PUSH EDI										;push EDI because the next routine uses edi
	
	INVOKE String_length, ESI						;get the length of string2
	MOV ECX, EAX									;move the length of string2 into ECX
	POP EDI											;restore EDI
	
L1:
	MOV BL, [ESI]									;move the nth element of string2 into BL
	CMP BL, [EDI]									;compare nth element to the n+pos'th element
	JNE NOTEQL										;jump if not equal
	INC ESI											;increment ESI
	INC EDI											;increment EDI
	LOOP L1											;loop to L1
	
	MOV AL, 1										;move 1 into AL (true condition)
	JMP RETURN										;jump to return
	
NOTEQL:
	MOV AL, 0										;move 0 into AL (false condition)
	
RETURN:	
	POP EBP
	RET
String_startsWith_1 ENDP


;----------------------------------------------------------------------------------------------------------
String_startsWith_2 proc PUBLIC
;
;		This sub routine checks if string1 starts with string2 If the comparison is true a 1 will be 
;	returned into the AL register, otherwise a zero will be returned.
;
;	Receives the addresses of string1, string2, and an integer
;	Returns a 1 or 0 to the AL register
;-----------------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP+8]	
intStr2Addr EQU [EBP+12]

	PUSH EBP										;save EBP
	MOV EBP, ESP									;set EBP=ESP
	
	MOV EDI, intStrAddr								;move string1 address into EDI
	MOV ESI, intStr2Addr							;move string2 address into ESI
	PUSH EDI										;push EDI because the next routine uses edi
	
	INVOKE String_length, ESI						;get the length of string2
	MOV ECX, EAX									;move the length of string2 into ECX
	POP EDI											;restore EDI
	
L1:
	MOV BL, [ESI]									;move the nth element of string2 into BL
	CMP BL, [EDI]									;compare nth element to the nth element
	JNE NOTEQL										;jump if not equal
	INC ESI											;increment ESI
	INC EDI											;increment EDI
	LOOP L1											;loop to L1
	
	MOV AL, 1										;move 1 into AL (true condition)
	JMP RETURN										;jump to return
	
NOTEQL:
	MOV AL, 0										;move 0 into AL (false condition)
	
RETURN:	
	POP EBP
	RET
String_startsWith_2 ENDP


;--------------------------------------------------------------------------------------------------------------
String_endsWith proc PUBLIC
;
;		This sub routine will check if string2 is equal to the suffix of string1.  If a match if is found 
;	a 1 will be returned in the AL register.
;
;	Receives the address of two strings from the stack
;	Returns a 1(true condition) or 0(false condition) to the AL register
;--------------------------------------------------------------------------------------------------------------
intStrAddr EQU [EBP+8]
intStr2Addr EQU [EBP+12]

	PUSH EBP										;save EBP
	MOV EBP, ESP									;set EBP=ESP
	
	MOV EDI, intStrAddr								;move string1 address into EDI
	MOV ESI, intStr2Addr							;move string2 address into ESI
	PUSH EDI										;push EDI because the next routine uses edi
	
	INVOKE String_length, intStr2Addr				;get length of intStr2Addr
	MOV EBX, EAX									;move the length into EBX
	PUSH EBX										;push EBX
	
	INVOKE String_length, intStrAddr				;get the length of int Str2Addr
	POP EBX											;pop EBX
	
	SUB EAX, EBX									;EAX - EBX
	PUSH EAX										;push EAX
	
	INVOKE String_length, ESI						;get the length of string2
	MOV ECX, EAX									;move the length of string2 into ECX
	POP EAX
	POP EDI											;restore EDI
	
	ADD EDI, EAX									;go to the beginning of the suffix
L1:
	MOV BL, [ESI]									;move the nth element of string2 into BL
	CMP BL, [EDI]									;compare nth element to the nth element
	JNE NOTEQL										;jump if not equal
	INC ESI											;increment ESI
	INC EDI											;increment EDI
	LOOP L1											;loop to L1
	
	MOV AL, 1										;move 1 into AL (true condition)
	JMP RETURN										;jump to return
	
NOTEQL:
	MOV AL, 0										;move 0 into AL (false condition)
	
RETURN:
	POP EBP
	RET
String_endsWith ENDP
END








