#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SHITM' 'WK0128' '((15,4,N),(121,4,N),(31,4,N),(125,4,N),(37,4,N))' '' '' '' '' '' '' '          �x����`�@�����@���o          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((5,4,N),(1,4,N))' '' '' '' '' '' '' '      �����ʎx����`�@�T�C�g�ꗗ�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
