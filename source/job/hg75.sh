#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0256' '((248,2,N),(250,1,N),(244,3,N),(7,6,N))' '' '' '' '' '' '' '          品名サイズ別　受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG720 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
