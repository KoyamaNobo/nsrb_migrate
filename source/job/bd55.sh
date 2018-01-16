#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSHF1' 'WK0768' '((512,8,N),(1,10,N),(19,8,N),(11,8,N))' '' '' '' '' '' '' '         î[ä˙ï Å@î≠íçÅEì¸å…ñæç◊ï\       '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD710 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
