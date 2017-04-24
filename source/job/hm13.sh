#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
NFCNV 'MN1=C MN2=DA PA1=MSD PA3=WK0064000 PA4=1 PA5=N4,C6,N1,N5,C48 \
PB1=../tmp/thtw.csv PB2=CSV2 PB3=PROTECT PB4=COMMA PB7=S,C,S,S,C \
PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HN011M $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
