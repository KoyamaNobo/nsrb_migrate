#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTMYR' 'WK0128' '((1,4,N),(123,6,N))' '' '' '' '' '' '' '���Ӑ挎�ʁ@�N�Ԕ�����z���ו\�@�i�o���p'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
