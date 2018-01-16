#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HKG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '11' 'WK0128000' '((1,36,C),(37,4,C))' '' '' '' '' '' '' '          êøãÅèëçTÇ¶Å@ñ⁄éüçÏï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=C36,C4,J26,C8,C6,C2,C20 \
PB1=../tmp/SEIKYU.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,C,C,C,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
