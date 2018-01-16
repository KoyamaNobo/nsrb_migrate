#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0256' '(7,7,N)' '' '' '' '' '' '' '           óöï®ç›å…ï\Å@ñ‚çáÇπ           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT110 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
