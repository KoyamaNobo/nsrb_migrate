#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=TANAOROSI PA4=1 \
PA5=C6,C1,C8,C1,C6,C16,J24,C6,C4,S4,C8,C6 PB1=../tmp/tana.csv \
PB2=CSV2 PB3=PROTECT PB4=COMMA PB7=C,C,C,C,C,C,J,C,C,S,C,C PB9=COPY \
MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
