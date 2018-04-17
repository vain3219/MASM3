
setString PROTO stringAddr:DWORD

String_equals PROTO intStr1Addr:DWORD, intStr2Addr:DWORD

String_equalsIgnoreCase PROTO intStr1Addr:DWORD, intStr2Addr:DWORD

String_copy PROTO intStr1Addr:DWORD

String_substring_1 PROTO intStrAddr:DWORD, intBeginInd:DWORD, intEndInd:DWORD

String_substring_2 PROTO intStrAddr:DWORD, intStartInd:DWORD

String_charat PROTO intStrAddr:DWORD, intStrLength:DWORD

String_startsWith_1 PROTO intStrAddr:DWORD, intStr2Addr:DWORD, intPos:DWORD

String_startsWith_2 PROTO intStrAddr:DWORD, intStr2Addr:DWORD

String_endsWith PROTO intStrAddr:DWORD, intStr2Addr:DWORD