#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMG450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,10,N),(32,7,N),(11,6,N))' '' '' '((17,5),(22,5),(27,5))' '' '' '' '            óöï®Å@óaÇËéÛï•ï\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
