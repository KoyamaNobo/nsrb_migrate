#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSHF1' 'WK0768' '((1,4,N),(5,6,N),(512,8,N))' '' '' '' '' '' '' '        �i���ʁ@�������Ɏc���ו\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD750 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
