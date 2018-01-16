#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/KHG570 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,5,C),(6,6,N))' '' '' '((12,6),(18,6),(24,6),(30,6),(36,6))' '' '' '' '         工品　製品受払　問合せ         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
