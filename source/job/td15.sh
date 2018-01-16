#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/TSD200 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TIDM' 'WK0064' '((7,6,N),(5,2,N),(25,4,N),(1,4,N))' '((1,42),(@          @),(1,12))' '' '' '' '' '' '          éÛéËàŸìÆì¸óÕÅ@ÉäÉXÉg          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSD210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
