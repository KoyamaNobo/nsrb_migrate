#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=HAIK-K PA4=1 PA5=C5,N6V2,C8 \
PB1=../tmp/haik-k.csv PB2=CSV2 PB3=PROTECT PB4=COMMA PB7=C,S,C PB9=COPY \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/KHY910 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
