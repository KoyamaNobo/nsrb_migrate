#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NFTORI-M' 'WK0064' '((26,2,C),(1,5,N),(6,20,C))' '((1,42),(@          @),(1,12))' '' '' '' '' '' '         取引先マスタ　コード表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PR072L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
