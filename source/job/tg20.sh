#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '((62,4,N),(153,6,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '          äÑà¯éËå`Å@åàçœó\íËï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG220 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
