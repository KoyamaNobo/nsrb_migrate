#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'RSF' 'WK0064' '((13,10,N),(37,2,N),(62,3,N))' '' '' '' '' '' '' '         �̎����@�`�F�b�N���X�g         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSR020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
