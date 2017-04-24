#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=WK1301 PA4=1 \
PA5=C3,C13,C4,C13,C4,C12,C4,C80,C255,C5,C14,C13,C4,C1,C13,C13,C13,C80,C8,C8\
,C8,C1,C1,C1,C3,C3,C1,C1,N4,C1,C1,C1,N4,C1,J20,C20,N9,N1,N5,C1,J20,C20,N5,C\
1,J20,C20,N5,C1,C1,C1,C1,C1,C1,N6,C1,J20,C20,N6,C1,J20,C20,C1,C1,C1,N2,N2,C\
1,N2,C1,C1,C1,C1,C1,C1,C1,C1,C3,C1,C10,C10,C10,C1,C1,C1,C1,N2,N2,N2,C1,C1,C\
1,N2,N2,C1,C1,C1,N2,C1,C1,C1,C1,C1,C1,C1,C1,C1,N2,N1,C1,C1,C3,C6,C1,C1,C1,C\
1,C14,C14,C3,C5,J25,C25,C16,C1,C4,J8,C8,C4,J8,C8,C1,C1,C1,C1,C1,C1,C1,C1,C1\
,C1,C1,C1,C1,C1,C1,C1,C1,C1,C1,N10,N7V2,N10,N7,N1,N4,C2,N2,N5V1,N4,C1,C1,C1\
,C1 PB1=../tmp/wmjcf.csv PB2=CSV2 PB3=PROTECT PB4=COMMA \
PB7=C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,S,C,C,C,S,C,J,C\
,S,S,S,C,J,C,S,C,J,C,S,C,C,C,C,C,C,S,C,J,C,S,C,J,C,C,C,C,S,S,C,S,C,C,C,C,C,\
C,C,C,C,C,C,C,C,C,C,C,C,S,S,S,C,C,C,S,S,C,C,C,S,C,C,C,C,C,C,C,C,C,S,S,C,C,C\
,C,C,C,C,C,C,C,C,C,J,C,C,C,C,J,C,C,J,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,\
C,S,S,S,S,S,S,C,S,S,S,C,C,C,C PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
