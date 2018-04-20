;----------------------------------------------------------------------------------------------------
;	FILE NAME : masm3.asm
;----------------------------------------------------------------------------------------------------
;
;		Program Name	:	MASM3
;		Programmer		:	Cody Thompson 
;		Class			:	CS 3B || Asm Lang
;		Date			:	4/02/2018
;		Purpose			:
;		This program will display a menu list of String-class-like operations that will be executed.
;	After a menu choice is input and executed the menu will be updated to reflect the result of the 
;	operation.
;----------------------------------------------------------------------------------------------------

	.486

	;Includes
	include ..\..\Irvine\Irvine32.inc
	include string1.inc
	include string2.inc
	
	;Prototypes
	getstring	PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
	putstring	PROTO Near32 stdcall, lpStringToPrint:dword
	ascint32 	PROTO Near32 stdcall, lpStringOfNumericChars:dword
	intasc32	proto Near32 stdcall, lpStringToHold:dword, dval:dword
	ExitProcess PROTO, dwExitCode:dword
	
	;External Prototypes
	EXTERN setString@0:PROC
	EXTERN String_equals@0:PROC
	EXTERN String_equalsIgnoreCase@0:PROC
	EXTERN String_copy@0:PROC
	EXTERN String_substring_1@0:PROC
	EXTERN String_substring_2@0:PROC
	EXTERN String_charat@0:PROC
	EXTERN String_startsWith_1@0:PROC
	EXTERN String_startsWith_2@0:PROC
	EXTERN String_endsWith@0:PROC
	EXTERN String_indexOf_1@0:PROC
	EXTERN String_indexOf_2@0:PROC
	EXTERN String_indexOf_3@0:PROC
	EXTERN String_lastIndexOf_1@0:PROC
	EXTERN String_lastIndexOf_2@0:PROC
	EXTERN String_lastIndexOf_3@0:PROC
	EXTERN String_replace@0:PROC
	EXTERN String_concat@0:PROC
	EXTERN String_toLowerCase@0:PROC
	EXTERN String_toUpperCase@0:PROC
	
	;Constants


	;Data segment
	.data
strString1		BYTE "NULL                           ", 0
strString2 		BYTE "NULL                           ", 0

strMenu 		BYTE 		"*********************************************************", 0Ah
strMenu1		BYTE 		"*                       MASM 3                          *", 0Ah
strMenu2		BYTE 		"* ------------------------------------------------------- ", 0Ah
strMenu3		BYTE 		"* <1> Set String1                                currently:", 0
dStr1Ptr		DWORD OFFSET strString1															
strMenu4		BYTE 0Ah, 	"* <2> Set String2                                currently:", 0
dStr2Ptr		DWORD OFFSET strString2
strMenu5		BYTE 0Ah,	"* <3> String_length (string1)                    currently:", 0
bLength			BYTE 30h, 4 dup(0)
strMenu6		BYTE 0Ah,   "* <4> String_equals (string1, string2)           currently:", 0
dEqualsPtr		DWORD OFFSET strFalse
strMenu7		BYTE 0Ah,   "* <5> String_equalsIgnoreCase(string1, string2)  currently:", 0
dCaseEqualsPtr	DWORD OFFSET strFalse
strMenu8		BYTE 0Ah,   "* <6> String_copy(string1)                       &", 0
strMenu8a		BYTE "  currently:", 0
dStr6Ptr		DWORD OFFSET strNull
strMenu9		BYTE 0Ah,   "* <7> String_substring_1                         &", 0
strMenu9a		BYTE "  currently:", 0
dStr7Ptr		DWORD OFFSET strNull
strMenu10		BYTE 0Ah,   "* <8> String_substring_2                         &", 0
strMenu10a		BYTE "  currently:", 0
dStr8Ptr		DWORD OFFSET strNull
strMenu11		BYTE 0Ah,   "* <9> String_charAt                              currently:", 0
strCharAt 		BYTE 2 DUP(0)
dPtrCharAt		DWORD OFFSET strNull
strMenu12		BYTE 0Ah,   "* <10> String_startsWith_1                       currently:", 0
dStarts1Ptr		DWORD OFFSET strFalse
strMenu13		BYTE 0Ah,   "* <11> String_startsWith_2                       currently:", 0
dStarts2Ptr		DWORD OFFSET strFalse
strMenu14		BYTE 0Ah,   "* <12> String_endsWith                           currently:", 0
dEndsPtr		DWORD OFFSET strFalse
strMenu15		BYTE 0Ah,   "* <13> String_indexOf_1                          currently:", 0
dIndex1			DWORD -1
strMenu16		BYTE 0Ah,   "* <14> String_indexOf_2                          currently:", 0
dIndex2			DWORD -1
strMenu17		BYTE 0Ah,   "* <15> String_indexOf_3                          currently:", 0
dIndex3			DWORD -1
strMenu18		BYTE 0Ah,   "* <16> String_lastIndexOf_1                      currently:", 0
dLastIndex1		DWORD -1
strMenu19		BYTE 0Ah,   "* <17> String_lastIndexOf_2                      currently:", 0
dLastIndex2		DWORD -1
strMenu20		BYTE 0Ah,   "* <18> String_lastIndexOf_3                      currently:", 0
dLastIndex3		DWORD -1
strMenu21		BYTE 0Ah,   "* <19> String_concat                             currently:", 0
dConcatPtr		DWORD OFFSET strNull
strMenu22		BYTE 0Ah,   "* <20> String_replace                            currently:", 0
dReplacePtr		DWORD OFFSET strNull
strMenu23		BYTE 0Ah,   "* <21> String_toLowerCase                        currently:", 0
dLowerPtr		DWORD OFFSET strNull
strMenu24		BYTE 0Ah,   "* <22> String_toUpperCase                        currently:", 0
dUpperPtr		DWORD OFFSET strNull
strMenu25		BYTE 0Ah,   "* <23> Quit                                               *", 0Ah
strMenu26		BYTE        "***********************************************************"
strChoice		BYTE 0Ah, "Choice (1-23): ", 0

strInput		BYTE 3 DUP(?), 0
dChoice			DWORD ?
dlength			DWORD 0
strFalse		BYTE "FALSE", 0
strTrue			BYTE "TRUE", 0 
strErrChoice	BYTE 0Ah, 0Ah, "The desired choice does not exist, re-enter your choice.", 0Ah, 0
strNull			BYTE "NULL", 0
strStringSelect BYTE "Which string do you want the length of( 1 or 2): ", 0
strSelection    BYTE 2 DUP(0)

strCharAtPrompt				BYTE 	"Please input an index: ", 0	
strSubStringPrompt			BYTE 	"Please input a starting index: ", 0
strSubStringPrompt2			BYTE 	0Ah, "Please input an ending index: ", 0
strStartInd					BYTE 	5 DUP(0)
strEndInd					BYTE 	5 DUP(0)
intStartInd					DWORD 	?
intEndInd					DWORD	?
strOutOBounds				BYTE 	0Ah, "The specified index is out of bounds, please re-enter your selection", 0Ah, 0
strCharInd					BYTE 	5 DUP(0)
intCharInd					DWORD	?


strNewLn		BYTE 0Ah, 0
	;Code segment
	.code
main proc								;start of main ;start of program
	MOV EAX, 0								;arbitrary

START:		
	CALL menu								;call the menu sub routine
	
	CMP dChoice, 23							;compare dChoice value to 23
	JE QUIT									;if dChoice is equal to 23 then jump to QUIT
	
	CALL getSubRoutine						;call the getSubRoutine sub routine
	
	JMP START								;jump to START
	
QUIT:	
	INVOKE ExitProcess,0				;terminate program
main ENDP								;end of main procedure

;----------------------------------------------------------------------------------------------------
menu proc
;
;		Outputs the menu to the console and waits for input.  The choice is converted from ascii to 
;	int, for simpler comparison, and then validated.  The choice is then stored into memory labeled 
;	'intChoice'.
;
;	Receives nothing
;	Returns nothing
;----------------------------------------------------------------------------------------------------
	CALL Clrscr								;clear the screen
	CALL menuOut

GET:
	INVOKE getstring, addr strInput, 3		;get the string from the console and store it into memory labeled 'strInput'
	INVOKE ascint32, addr strInput			;convert the string from ascii values to integer values
	MOV dChoice, EAX						;move the value resulting from ascint32 to memory labeled dChoice
	
	cmp dChoice, 23							;compare dChoice to 23
	JE RETURN								;jump to RETURN if the comparison is equal
	JG INVALID								;jump to INVALID if the comparison is greater than 23
	
	CMP dChoice, 1							;compare dChoice to 1
	JL INVALID								;jump to invalid if the comparison is less than 1
	JMP RETURN								;jump to RETURN
	
INVALID:
	MOV EDX, OFFSET strErrChoice			;move the offset address of strErrChoice into EDX
	CALL WriteString						;call WriteString sub routine
	MOV EDX, OFFSET strChoice				;move the offset address of strChoice into EDX
	CALL WriteString						;call WriteString sub routine
	JMP GET									;jump to GET
	
RETURN:
	RET										;return
menu ENDP								;end of menu

;----------------------------------------------------------------------------------------------------
menuOut proc
;
;		Outputs the menu to the console and waits for input.  The choice is converted from ascii to 
;	int, for simpler comparison, and then validated.  The choice is then stored into memory labeled 
;	'intChoice'.
;
;	Receives nothing
;	Returns nothing
;----------------------------------------------------------------------------------------------------
	
	INVOKE putstring, addr strMenu
	INVOKE putstring, dStr1Ptr
	INVOKE putstring, addr strMenu4
	INVOKE putstring, dStr2Ptr
	INVOKE putstring, addr strMenu5
	INVOKE putstring, addr bLength
	INVOKE putstring, addr strMenu6
	INVOKE putstring, dEqualsPtr
	INVOKE putstring, addr strMenu7
	INVOKE putstring, dCaseEqualsPtr
	INVOKE putstring, addr strMenu8
	CMP dStr6Ptr, OFFSET strNull
	JNE LSIXA
	MOV EAX, 0h
	JMP LSIXB
LSIXA:
	MOV EAX, dStr6Ptr
LSIXB:	
	CALL WriteHex
	INVOKE putstring, addr strMenu8a
	INVOKE putstring, dStr6Ptr
	INVOKE putstring, addr strMenu9
	CMP dStr7Ptr, OFFSET strNull
	JNE LSEVA
	MOV EAX, 0h
	JMP LSEVB
LSEVA:
	MOV EAX, dStr7Ptr
LSEVB:	
	CALL WriteHex
	INVOKE putstring, addr strMenu9a
	INVOKE putstring, dStr7Ptr
	INVOKE putstring, addr strMenu10
	CMP dStr8Ptr, OFFSET strNull
	JNE LEIGHTA
	MOV EAX, 0h
	JMP LEIGHTB
LEIGHTA:
	MOV EAX, dStr8Ptr
LEIGHTB:	
	CALL WriteHex
	INVOKE putstring, addr strMenu10a
	INVOKE putstring, dStr8Ptr
	INVOKE putstring, addr strMenu11
	INVOKE putString, dPtrCharAt
	INVOKE putstring, addr strMenu12
	INVOKE putString, dStarts1Ptr
	INVOKE putstring, addr strMenu13
	INVOKE putString, dStarts2Ptr
	INVOKE putstring, addr strMenu14
	INVOKE putString, dEndsPtr
	INVOKE putstring, addr strMenu15
	MOV EAX, dIndex1
	CALL WriteInt
	INVOKE putstring, addr strMenu16
	MOV EAX, dIndex2
	CALL WriteInt
	INVOKE putstring, addr strMenu17
	MOV EAX, dIndex3
	CALL WriteInt	
	INVOKE putstring, addr strMenu18
	MOV EAX, dLastIndex1
	CALL WriteInt
	INVOKE putstring, addr strMenu19
	MOV EAX, dLastIndex2
	CALL WriteInt
	INVOKE putstring, addr strMenu20
	MOV EAX, dLastIndex3
	CALL WriteInt
	INVOKE putstring, addr strMenu21
	INVOKE putstring, dConcatPtr
	INVOKE putstring, addr strMenu22
	INVOKE putstring, dReplacePtr
	INVOKE putstring, addr strMenu23
	INVOKE putstring, dLowerPtr
	INVOKE putstring, addr strMenu24
	INVOKE putstring, dUpperPtr
	INVOKE putstring, addr strMenu25
	
	RET
menuOut ENDP


;----------------------------------------------------------------------------------------------------
getSubRoutine proc
;
;		This sub routine will first push the offset addresses for strString1 and strString2 on to the 
;	stack and then call the appropriate sub routine.  The choice that was input from the menu sub routine
;	will be moved into EAX and compared against hard coded values to call the specified sub routine.
;
;	Receives nothing 
;	Returns nothing
;----------------------------------------------------------------------------------------------------
	MOV EAX, OFFSET strString2				;move the offset address of strString2 into EAX
	MOV EBX, OFFSET strString1				;move the offset address of strString1 into EBX
	
	MOV EDX, dChoice						;move dChoice into EDX for if statements
	
	.if EDX == 1;---------------------------	
	PUSH EBX								;push EBX
	CALL setString@0						;call setString@4
	ADD ESP, 4								;add 4 bytes to ESP
	
	JMP RETURN
	
	.elseif EDX == 2;-----------------------
	PUSH EAX								;push EAX
	CALL setString@0						;call setString@4
	ADD ESP, 4								;add 4 bytes to ESP
	
	JMP RETURN
	
	.elseif EDX == 3;-----------------------
	CALL Clrscr
	
	INVOKE putString, addr strStringSelect	;display prompt
	INVOKE getString, addr strSelection, 3	;get input
	
	CMP strSelection, 32h					;compare to ascii code for integer 2
	JE STR2
	
	PUSH EBX								;push EBX
	JMP LENGTHCALL							;jump to call
STR2:
	MOV EAX, OFFSET strString2				;move the offset address of strString2 into EAX
	PUSH EAX								;push EAX
	
LENGTHCALL:
	CALL String_length						;call String_length
	INVOKE intasc32, addr bLength, EAX		;convert integer into ascii characters
	
	JMP RETURN
	
	.elseif EDX == 4;-----------------------
	MOV EDX, OFFSET strFalse				;move the offset address of strFalse into EDX
	MOV dEqualsPtr, EDX						;set dEqualsPtr equal to EDX
	
	PUSH EAX								;push EAX
	PUSH EBX								;push EBX
	CALL String_equals@0					;call String_equals@8
	ADD ESP, 8
	
	CMP AL, 0								;compare AL to zero
	JE J4									;jump to J4 if AL == 0
	MOV EDX, OFFSET strTrue					;move the offset address of strTrue into EDX
	MOV dEqualsPtr, EDX						;set dEqualsPtr equal to EDX
J4:
	JMP RETURN
	
	.elseif EDX == 5;-----------------------
	MOV EDX, OFFSET strFalse				;move the offset address of strFalse into EDX
	MOV dCaseEqualsPtr, EDX					;set dCaseEqualsPtr equal to EDX
	
	PUSH EAX								;push EAX
	PUSH EBX								;push EBX
	CALL String_equalsIgnoreCase@0			;call String_equalsIgnoreCase
	ADD ESP, 8
	
	CMP AL, 0								;compare AL to zero
	JE J5									;jump to J5 if AL == 0
	MOV EDX, OFFSET strTrue					;move the offset address of strTrue into EDX
	MOV dCaseEqualsPtr, EDX					;set dCaseEqualsPtr equal to EDX
J5:
	JMP RETURN
	
	.elseif EDX == 6;-----------------------
	PUSH EBX								;push EBX to the stack
	CALL String_copy@0						;call String_copy@4
	ADD ESP, 4
	MOV dStr6Ptr, EAX						;move new address into memory
	
	JMP RETURN
	
	.elseif EDX == 7;-----------------------
	CALL Clrscr										;clear the screen
	
	PROMPT7a:
	MOV EBX, 0										;move 0 into EBX
	INVOKE putString, addr strSubStringPrompt		;write prompt to the console
	INVOKE getString, addr strStartInd, 3			;get input from the console
	INVOKE ascint32, addr strStartInd				;convert ascii values to real integer values
	MOV ECX, EAX									;move result into ECX
	
	CMP ECX, 0										;compare result to 1
	JL OUTOFBOUNDS7									;jump to OUTOFBOUNDS if the input value is >1
	MOV ESI, OFFSET strString1						;move the offset address of strString1 into ESI
	PUSH ESI										;push ESI
	CALL String_length								;get length of [intStrAddr]	
	CMP ECX, EAX									;compare result to stringLrngth
	JG OUTOFBOUNDS7									;jump to OUTOFBOUNDS if the input value > length
	
PROMPT7b:										;move 1 into EBX
	INVOKE putString, addr strSubStringPrompt2		;write prompt to the console							;call WriteString
	INVOKE getString, addr strEndInd, 3				;get the input from the console
	INVOKE ascint32, addr strEndInd					;convert ascii values to real integer values
	MOV intEndInd, EAX								;move result of conversion into intEndInd
	MOV EDX, EAX									;move result into EDX

	MOV EBX, 1	
	CMP EDX, 0										;compare result to 1
	JL OUTOFBOUNDS7									;jump to OUTOFBOUNDS if the input value is >1
	PUSH ESI										;push ESI
	CALL String_length								;get length of [intStrAddr]	
	CMP EDX, EAX									;compare result to stringLrngth
	JG OUTOFBOUNDS7									;jump to OUTOFBOUNDS if the input value > length
	
	PUSH EDX										;push EDX
	PUSH ECX										;push ECX
	PUSH ESI										;PUSH ESI
	CALL String_substring_1@0						;call String_substring_1@8
	ADD ESP, 12
	MOV dStr7Ptr, EAX								;move resulting address to dStr7Ptr
	
	JMP RETURN
	
OUTOFBOUNDS7:
	INVOKE putString, addr strOutOBounds			;output out of bounds message to the console
	CMP EBX, 0										;compare EBX to 0 (first or second prompt)
	JE PROMPT7a										;jump if equal to PROMPT1
	JMP PROMPT7b									;otherwise jump to PROMPT2
	
	.elseif EDX == 8;-----------------------
	CALL Clrscr										;call clear screen

	PROMPT8:
	MOV EBX, 0										;move 0 into EBX
	INVOKE putString, addr strSubStringPrompt		;write prompt to the console
	INVOKE getString, addr strStartInd, 3			;get input from the console
	INVOKE ascint32, addr strStartInd				;convert ascii values to real integer values
	MOV ECX, EAX									;move result into ECX
	
	CMP ECX, 0										;compare result to 1
	JL OUTOFBOUNDS8									;jump to OUTOFBOUNDS if the input value is >1
	MOV ESI, OFFSET strString1						;move the offset address of strString1 into ESI
	PUSH ESI										;push ESI
	CALL String_length								;get length of [intStrAddr]	
	CMP ECX, EAX									;compare result to stringLrngth
	JG OUTOFBOUNDS8									;jump to OUTOFBOUNDS if the input value > length
	

	PUSH ECX										;push ECX
	PUSH ESI										;push ESI
	CALL String_substring_2@0						;call String_substring_1@8
	ADD ESP, 8
	MOV dStr8Ptr, EAX								;move new address into memory
	
	JMP RETURN
	
OUTOFBOUNDS8:
	INVOKE putString, addr strOutOBounds			;output the out of bounds message to the console
	JMP PROMPT8
	
	.elseif EDX == 9;-----------------------
	CALL Clrscr											;call clear screen
	
	INVOKE putString, addr strCharAtPrompt			;write index prompt to the console
	INVOKE getString, addr strCharInd, 3			;get input from the console
	INVOKE ascint32, addr strCharInd				;convert ascii values to real integer values
	
	PUSH EAX										;push EAX
	PUSH EBX										;push ESI
	CALL String_charat@0							;call String_charat
	ADD ESP, 8										
	
	CMP AL, 0
	JE RETURN
	MOV dPtrCharAt, OFFSET strCharAt				;move char address into memory
	MOV strCharAt, AL
	
	JMP RETURN
	
	.elseif EDX == 10;-----------------------
	CALL Clrscr
	MOV EDX, OFFSET strFalse						;move the offset address of strFalse into EDX
	MOV dStarts1Ptr, EDX							;set dEqualsPtr equal to EDX
	
	INVOKE putString, addr strCharAtPrompt			;write index prompt to the console
	INVOKE getString, addr strCharInd, 3			;get input from the console
	INVOKE ascint32, addr strCharInd				;convert ascii values to real integer values
	
	PUSH EAX										;push EAX
	MOV EAX, OFFSET strString2						;move the offset address of strString2 into EAX
	PUSH EAX										;push EAX
	PUSH EBX										;push ESI
	CALL String_startsWith_1@0						;call String_startsWith_1
	ADD ESP, 12
	
	CMP AL, 0										;compare AL to zero
	JE J10											;jump to J4 if AL == 0
	MOV EDX, OFFSET strTrue							;move the offset address of strTrue into EDX
	MOV dStarts1Ptr, EDX							;set dEqualsPtr equal to EDX
J10:
	JMP RETURN
	
	.elseif EDX == 11;-----------------------
	CALL Clrscr
	MOV EDX, OFFSET strFalse						;move the offset address of strFalse into EDX
	MOV dStarts2Ptr, EDX							;set dEqualsPtr equal to EDX
	
	PUSH EAX										;push EAX
	PUSH EBX										;push ESI
	CALL String_startsWith_2@0						;call String_startsWith_1
	ADD ESP, 8
	
	CMP AL, 0										;compare AL to zero
	JE J11											;jump to J4 if AL == 0
	MOV EDX, OFFSET strTrue							;move the offset address of strTrue into EDX
	MOV dStarts2Ptr, EDX							;set dEqualsPtr equal to EDX
J11:
	JMP RETURN
	
	.elseif EDX == 12;-----------------------
	CALL Clrscr
	MOV EDX, OFFSET strFalse						;move the offset address of strFalse into EDX
	MOV dEndsPtr, EDX								;set dEqualsPtr equal to EDX
	
	PUSH EAX										;push EAX
	PUSH EBX										;push ESI
	CALL String_endsWith@0							;call String_startsWith_1
	ADD EBP, 8
	
	CMP AL, 0										;compare AL to zero
	JE J12											;jump to J4 if AL == 0
	MOV EDX, OFFSET strTrue							;move the offset address of strTrue into EDX
	MOV dEndsPtr, EDX								;set dEqualsPtr equal to EDX
J12:
	JMP RETURN
	
	.elseif EDX == 13;-----------------------
	Call CLrscr
	push EBX
	call String_indexOf_1
	add esp, 4
	mov dIndex1, EAX
	JMP RETURN
	
	.elseif EDX == 14;-----------------------
	Call CLrscr
	push offset strString1
	call String_indexOf_2
	add esp, 4
	mov dIndex2, eax
	JMP RETURN
	
	.elseif EDX == 15;-----------------------
	Call CLrscr
	push offset strString2
	push offset strString1
	call String_indexOf_3
	add esp, 8
	mov dIndex3, eax
	JMP RETURN
	
	.elseif EDX == 16;-----------------------
	Call CLrscr
	push offset strString1
	call String_lastIndexOf_1
	add esp, 4
	mov dLastIndex1, EAX
	JMP RETURN
	
	.elseif EDX == 17;-----------------------
	Call CLrscr
	push offset strString1
	call String_lastIndexOf_2
	add esp, 4
	mov dLastIndex2, EAX
	
	JMP RETURN
	
	.elseif EDX == 18;-----------------------
	call CLrscr
	push offset strString2
	push offset strString1
	call String_lastIndexOf_3
	add esp, 8
	mov dLastIndex3, eax
	JMP RETURN
	
	.elseif EDX == 19;-----------------------
	call CLrscr
	push offset strString2
	push offset strString1
	call String_concat
	add esp, 8
	mov dConcatPtr, EAX
	JMP RETURN
	
	.elseif EDX == 20;-----------------------
	call CLrscr
	push offset strString2
	push offset strString1
	call String_replace
	add esp, 8
	mov dReplacePtr, EAX
	JMP RETURN
	
	.elseif EDX == 21;-----------------------
	call CLrscr
	push EAX
	push EBX
	call String_toLowerCase@0
	add esp, 8
	mov dLowerPtr, EAX
	JMP RETURN
	
	.elseif EDX == 22;-----------------------
	call CLrscr
	push EAX
	push EBX
	call String_toUpperCase@0
	add esp, 8
	mov dUpperPtr, EAX
	JMP RETURN
	
	.endif
	
RETURN:
	RET										;return
getSubRoutine ENDP						;end of getSubRoutine

;----------------------------------------------------------------------------------------------------
String_length proc, intStrAddr:DWORD
;
;		This sub routine counts the number of elements in strStringX and stores the result in EAX.
;
;	Receives the address of strStringX
;	Returns the value in the EAX register
;----------------------------------------------------------------------------------------------------
	MOV EBX, 0								;clear the EBX register
	MOV EAX, 0								;clear the EAX register
	
	MOV EDI, intStrAddr						;move str1Addr into EDI for indirect addressing
	
L1:
	MOV BL, [EDI]							;move the nth element of strString1 into BL
	CMP BL, 0								;compare BL to 0
	JE RETURN								;jump to RETURN if the comparison is equal
	INC EAX									;increment EAX
	INC EDI									;increment string address to go to the next element
	JMP L1									;loop to L1
	
RETURN:
	RET										;return
String_length ENDP						;end of String_length

end main								;end of main