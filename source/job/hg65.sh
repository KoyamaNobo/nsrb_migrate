#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMG750 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((32,2,N),(36,1,N),(28,3,N),(1,4,N))' '' '' '' '' '' '' '          óöï®ç›å…ì¸å…ì˙ñæç◊ï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG760 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
