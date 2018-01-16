#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/KBG450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((57,2,N),(54,1,N),(1,6,N),(7,8,N),(15,1,N),(41,6,N))' '' '' '' '' '' '' '           çﬁóøì˙ïtï Å@éÛï•ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
