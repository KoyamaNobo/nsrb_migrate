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
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            óöï®Å@ç›å…ñæç◊ï\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD620 $USER_ID $JRCODE 6 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,S7,S7,C120 \
PB1=../tmp/HZAIKO.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
