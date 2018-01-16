#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMY265 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=HHUHF PA4=1 \
PA5=C6,C6,J24,S6,S9,S7,S10,S8,S10,S6,S9,S10,C3,C1,C2,C2,C1 \
PB1=../tmp/HHUHF.XLS PB2=SYLK PB3=SHARE \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,C,C,C,C,C MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
