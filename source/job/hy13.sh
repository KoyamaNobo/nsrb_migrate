#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMY060 $USER_ID $JRCODE 00
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'WK0064' 'WK0064' '((1,6,N),(53,4,N),(7,1,N))' '' '' '((8,4,P),(12,4,P),(16,4,P),(20,4,P),(24,4,P),(28,4,P),(32,4,' 'P),(36,4,P),(40,4,P),(44,4,P),(48,5,P))' '' '' '  年間品名得意先サイズ別　出荷数明細表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY070 $USER_ID $JRCODE 1 0 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
