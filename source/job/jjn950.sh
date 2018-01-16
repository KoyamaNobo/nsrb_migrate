#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HTIM' 'WK0256' '(7,9,N)' '((1,85),(@          @),(1,85),(1,76))' '' '((16,6),(22,6),(28,6),(34,6),(40,6),(46,6),(52,6),(58,6),(64' ',6),(70,6))' '' '' '             棚　卸　更　新             '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT090U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
