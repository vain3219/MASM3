;----------------------------------------------------------------------------------------------------
;	FILE NAME : masm3.asm
;----------------------------------------------------------------------------------------------------
;
;		Program Name	:
;		Programmer		:	Cody Thompson 
;		Class			:	CS 3B || Asm Lang
;		Date			:	
;		Purpose			:
;
;
;----------------------------------------------------------------------------------------------------

	.486

	;Includes
	include ..\..\Irvine\Irvine32.inc
	include string1.inc

	;Prototypes
	getstring	PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
	ascint32 	PROTO Near32 stdcall, lpStringOfNumericChars:dword
	EXTERN setString1@8:PROC
	EXTERN setString2@8:PROC
	ExitProcess PROTO, dwExitCode:dword

	;Constants


	;Data segment
	.data
strString1		BYTE 31 DUP(?), 0
strString2 		BYTE 31 DUP(?), 0

strMenu 		BYTE "*********************************************************", 0Ah
strMenu1		BYTE "*                       MASM 3                            *", 0Ah
strMenu2		BYTE "* ------------------------------------------------------- *", 0Ah
strMenu3		BYTE "* <1> Set String1   currently:<empty>                     *", strString1, 0Ah
strMenu4		BYTE "* <2> Set String2   currently:<empty>                     *", 0Ah
strMenu5		BYTE "* <3> String_length (string1)                             *", 0Ah
strMenu6		BYTE "* <4> String_equals (string1, string2)                    *", 0Ah
strMenu7		BYTE "* <5> String_equalsIgnoreCase(string1, string2)           *", 0Ah
strMenu8		BYTE "* <6> String_copy(string1)                                *", 0Ah
strMenu9		BYTE "* <7> String_substring_1                                  *", 0Ah
strMenu10		BYTE "* <8> String_substring_2                                  *", 0Ah
strMenu11		BYTE "* <9> String_charAt                                       *", 0Ah
strMenu12		BYTE "* <10> String_startsWith_1                                *", 0Ah
strMenu13		BYTE "* <11> String_startsWith_2                                *", 0Ah
strMenu14		BYTE "* <12> String_endsWith                                    *", 0Ah
strMenu15		BYTE "* <13> String_indexOf_1                                   *", 0Ah
strMenu16		BYTE "* <14> String_indexOf_2                                   *", 0Ah
strMenu17		BYTE "* <15> String_indexOf_3                                   *", 0Ah
strMenu18		BYTE "* <16> String_lastIndexOf_1                               *", 0Ah
strMenu19		BYTE "* <17> String_lastIndexOf_2                               *", 0Ah
strMenu20		BYTE "* <18> String_lastIndexOf_3                               *", 0Ah
strMenu21		BYTE "* <19> String_concat                                      *", 0Ah
strMenu22		BYTE "* <20> String_replace                                     *", 0Ah
strMenu23		BYTE "* <21> String_toLowerCase                                 *", 0Ah
strMenu24		BYTE "* <22> String_toUpperCase                                 *", 0Ah
strMenu25		BYTE "* <23> Quit                                               *", 0Ah
strMenu26		BYTE "***********************************************************", 0Ah
strChoice		BYTE 0Ah, "Choice (1-23):", 0

strInput		BYTE 3 DUP(?), 0
dChoice			DWORD ?

strErrChoice	BYTE "The desired choice does not exist, re-enter your choice.", 0Ah, 0

strNewLn		BYTE 0Ah, 0
	;Code segment
	.code
main proc								;start of main ;start of program
START:	
	MOV EAX, 0								;arbitrary
	
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
	MOV EDX, OFFSET strMenu					;move the offset address of strMenu into EDX
	CALL WriteString						;write the string stored at the address in EDX to the console

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
getSubRoutine proc
;
;
;
;
;----------------------------------------------------------------------------------------------------
	MOV EAX, OFFSET strString2				;move the offset address of strString2 into EAX
	PUSH EAX								;push EAX to the stack
	MOV EAX, OFFSET strString1				;move the offset address of strString1 into EAX
	PUSH EAX								;push EAX to the stack
	
	MOV EAX, dChoice						;move dChoice into EAX for if statements
	
	.if eax == 1							
	CALL setString1@8						;call setString1@8
	
	.elseif eax == 2
	CALL setString2@8						;call setString2@8

	.endif
	RET										;return
getSubRoutine ENDP						;end of getSubRoutine

end main								;end of main