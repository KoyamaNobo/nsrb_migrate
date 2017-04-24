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
../exec/JTN01U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=JMSTW PA4=1 \
PA5=N6,N1,N1,N8,C4,C3,C6,N1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,N3,N8,N5,C10,J\
9,J23,J26,J26,J24,C3 PB1=../tmp/JYUTYU.CSV PB2=CSV2 PB3=EXCLUSIVE \
PB4=COMMA PB7=S,S,S,S,C,C,C,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,C,J,J,J,J,J,C \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
