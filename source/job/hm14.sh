#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HN013M $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA2= PA3=WK0064000 PA4=1 PA5=C4,C6,C1,N5,C48 \
PB1=../tmp/thtw.csv PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,C,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
