#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMY010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((36,2,N),(15,4,N),(1,8,N),(38,1,N),(9,6,N))' '' '' '' '' '' '' '   担当得意先別　売上値引伝票　合計表   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
