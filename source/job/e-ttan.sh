#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=TTANAF PA4=1 \
PA5=C1,C6,J24,C4,S5,N3,S6,S6,S6,S6,C5 PB1=../tmp/t-tana.csv \
PB2=CSV2 PB3=PROTECT PB4=COMMA PB7=C,C,J,C,S,S,S,S,S,S,C PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
