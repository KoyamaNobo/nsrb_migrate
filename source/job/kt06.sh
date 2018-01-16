#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRYR' 'WK0128' '((14,5,C),(2,8,N),(10,4,N),(67,7,N))' '' '' '' '' '' '' '     品名別　売上・値引伝票　問合せ     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT050 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
