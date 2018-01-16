#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '((77,2,N),(1,2,N),(15,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '         ïîñÂçﬁóøï Å@édì¸ñæç◊ï\         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG430 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
