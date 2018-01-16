#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '((153,6,N),(62,4,N),(54,2,N),(18,4,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '        äÑà¯éËå`óéÇøçûÅ@êUë÷ì`ï[        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SHITM' 'WK0128' '((125,4,N),(37,4,N),(9,4,N),(15,4,N))' '' '' '' '' '' '' '        éxï•éËå`óéÇøçûÅ@êUë÷ì`ï[        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG720 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
