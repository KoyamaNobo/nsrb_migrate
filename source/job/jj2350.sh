#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT235U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((16,6,N),(1,8,N),(9,4,N),(22,7,N))' '' '' '' '' '' '' '       品名受注日別　受注残リスト       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT245L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
