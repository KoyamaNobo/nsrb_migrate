#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURF' 'WK0128' '(1,8,N)' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           ì˙ïtï Å@ì¸ã‡ñæç◊ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG350 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
