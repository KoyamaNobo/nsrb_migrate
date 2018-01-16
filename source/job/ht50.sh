#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           óöï®Å@ïiéÌï éÛï•ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
