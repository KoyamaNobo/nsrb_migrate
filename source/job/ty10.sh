#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '((18,4,N),(159,4,N),(34,4,N),(1,4,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '          éÊà¯êÊï Å@éÛéÊéËå`í†          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSY110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
