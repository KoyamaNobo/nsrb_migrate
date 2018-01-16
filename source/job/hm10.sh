#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HIM1' 'WK0256' '((64,1,N),(7,6,N))' '' '' '' '' '' '' '       履物　品名製造原価　リスト       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
