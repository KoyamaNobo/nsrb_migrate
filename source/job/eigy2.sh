#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '' '' '' '' '' '' '           óöï®Å@ïiéÌï éÛï•ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG000 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
