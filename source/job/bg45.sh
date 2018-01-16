#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/KBG410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JTM' 'WK0128' '(73,2,N)' '((1,85),(@          @),(1,33))' '' '' '' '' '' '        材料部門別　仕入・棚卸表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG420 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
