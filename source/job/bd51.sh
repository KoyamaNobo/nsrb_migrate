#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSHF1' 'WK0768' '((19,8,N),(1,10,N),(11,8,N))' '' '' '' '' '' '' '       î≠íçì˙ï Å@î≠íçÅEì¸å…ñæç◊ï\       '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD710 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
