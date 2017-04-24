#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
NFCNV 'MN1=C MN2=AD PA1=MSD PA2= PA3=FUKUF1 PA4=1 \
PA5=C15,C17,J20,J20,J20,J20,J20,C8,C5,C3,C12,N2,N3,N4,J15,J15,J15,C30\
,C30,C8,C16,C30,C1,N4,C8,C8 PB1=../tmp/FUKUTU.CSV PB2=CSV1 \
PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,J,J,J,J,C,C,C,C,S,S,S,J,J,J,C,C,C,C,C,C,S,C,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
