#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JHS42U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,5,N),(63,1,N),(13,7,N))' '' '' '((20,4),(24,4),(28,4),(32,4),(36,4),(40,4),(44,4),(48,4),(52' ',4),(56,4))' '' '' '       ナフコ　出荷指図　自動変換       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS47U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/JTN35L $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
ENDJOB:
