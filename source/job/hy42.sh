#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(1,10,N),(50,6,N))' '' '' '((11,7),(18,10),(28,10))' '' '' '' '�S�����Ӑ�i�� �N�Ԕ���W�v (���B���F���f�B)'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY360 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
