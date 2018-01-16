#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT210U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((1,8,N),(9,7,N),(16,6,N),(22,6,N),(28,1,N))' '' '' '' '' '' '' '         　納期別　出荷予定表         　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT221L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
