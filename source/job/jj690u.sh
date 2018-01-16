#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT910U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((2,8,N),(16,4,N),(42,3,N),(34,7,N))' '' '' '' '' '' '' '      受注日得意先別　受注残リスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT920L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
