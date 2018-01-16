#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRYR' 'WK0128' '((10,4,N),(2,8,N),(67,7,N))' '' '' '' '' '' '' '    得意先別　売上・値引伝票　問合せ    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT050 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
