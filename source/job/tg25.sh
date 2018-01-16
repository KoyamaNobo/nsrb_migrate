#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SHITM' 'WK0128' '((125,4,N),(37,4,N),(9,4,N),(121,4,N),(31,4,N),(1,4,N))' '' '' '' '' '' '' '           ä˙ì˙ï Å@éxï•éËå`í†           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG320 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
