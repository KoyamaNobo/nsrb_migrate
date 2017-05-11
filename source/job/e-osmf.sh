#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=OSMF PA4=1 PA5=C6,C8,C4,C3,C1,C1,N3,C2 \
PB1=./datadir/osmf.csv PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,C,C,C,C,S,C \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
