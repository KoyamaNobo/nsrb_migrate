#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(5,6,N))' '' '' '' '' '' '' '     �S���i��ʁ@�N�Ԕ���e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY330 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
