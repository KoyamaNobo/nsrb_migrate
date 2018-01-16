#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/TSD150 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TJSSF' 'WK0128' '((3,12,N),(1,2,N),(46,6,S))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '      éËå`Å@Å|Å|ÅÑÅ@çwîÉÅ@ÇkÇhÇmÇj      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSD160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
