#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTN27U $USER_ID $JRCODE 9
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((1,6,N),(128,1,N),(61,1,N))' '' '' '((62,6),(68,6),(74,6),(80,6),(86,6),(92,6),(98,6),(104,6),(1' '10,6),(116,6),(122,6))' '' '' '     óLå¯ç›å…  íäèo                     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN28U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=SYZDF PA4=1 \
PA5=C6,J24,J3,N1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6 PB1=../tmp/SYZDF.XLS \
PB2=SYLK PB3=SHARE PB7=C,J,J,S,S,S,S,S,S,S,S,S,S,S,S MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
