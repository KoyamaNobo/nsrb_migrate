#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYUF' 'WK0128' '((67,1,N),(9,4,N),(1,8,N),(51,8,C),(44,7,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '              �����[�@���s              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKD020 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKD050 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
