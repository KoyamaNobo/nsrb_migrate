#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JTM' 'WK0128' '((73,2,N),(1,6,N))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '           çﬁóøÅ@ç›å…Å@ñæç◊ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG570 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
