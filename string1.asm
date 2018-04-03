;----------------------------------------------------------------------------------------------------
;	FILE NAME : string1.asm
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

	;Prototypes
	getstring	PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
	putstring	PROTO Near32 stdcall, lpStringToPrint:dword
	ExitProcess PROTO, dwExitCode:dword

	;Constants


	;Data segment
	.data
strSetString		 		BYTE "Please input a string: ", 0
strString1					BYTE 33 DUP(?)
strString2					BYTE 33 DUP(?)
	;Code segment
	.code						;end of main procedure

;----------------------------------------------------------------------------------------------------
setString1 proc PUBLIC, intStr1Addr:DWORD, intStr2Addr:DWORD
;
;		Sets the value of String1.
;
;	Receives the offset address of strString1 from masm3.asm.
;	Returns nothing
;----------------------------------------------------------------------------------------------------
	CALL Clrscr
	
	INVOKE putString, addr strSetString
	INVOKE getString, intStr1Addr, 32
	
	RET
setString1 ENDP

;----------------------------------------------------------------------------------------------------
setString2 proc PUBLIC, intStr1Addr:DWORD, intStr2Addr:DWORD
;
;		Sets the value of String2
;
;	Receives the offset address of setString2 from masm3.asm
;	Returns nothing
;----------------------------------------------------------------------------------------------------
	CALL Clrscr
	
	INVOKE putString, addr strSetString
	INVOKE getString, intStr2Addr, 32
	
	RET
setString2 ENDP

END