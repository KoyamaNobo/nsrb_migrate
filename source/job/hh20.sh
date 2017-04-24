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
NFCNV 'MN1=C MN2=DA PA1=MSD PA3=WK0064000 PA4=1 \
PA5=N6,N1,S5,S5,S5,S5,S5,S5,S5,S5,S5,S5,C7 PB1=../tmp/haik.csv \
PB2=CSV2 PB3=PROTECT PB4=COMMA PB7=S,S,S,S,S,S,S,S,S,S,S,S,C PB9=COPY \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'WK0064000' 'WK0064' '(1,7,N)' '' '' '((8,5),(13,5),(18,5),(23,5),(28,5),(33,5),(38,5),(43,5),(48,' '5),(53,5))' '' '' '                                        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
