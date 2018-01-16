#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRYR' 'WK0064' '((11,5,C),(3,8,N),(1,2,N),(63,1,N))' '' '' '' '' '' '' '     ïiñºï Å@â¡ó∞ÅEîpãpì`ï[Å@ñ‚çáÇπ     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
