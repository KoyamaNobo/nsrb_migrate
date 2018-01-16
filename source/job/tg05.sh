#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TYBF' 'WK0064' '((1,4,N),(44,4,N),(7,4,N),(48,4,N),(13,4,N),(23,4,N))' '((1,51),(@          @),(1,3))' '' '' '' '' '' '           ã‚çsï Å@äÑà¯éËå`í†           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
