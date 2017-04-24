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
../exec/HMG750 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((32,2,N),(36,1,N),(28,3,N),(1,4,N))' '' '' '' '' '' '' '          óöï®ç›å…ì¸å…ì˙ñæç◊ï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG760 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=HZNPCF PA4=1 PA5=C4,J24,S7,N5,S9,C2 \
PB1=../tmp/HZNPCF.XLS PB2=SYLK PB3=EXCLUSIVE PB7=C,J,S,S,S,C MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
