#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'ZDF' 'WK0064' '((17,4,N),(1,6,N))' '((1,21),(@          @),(1,21),(1,12))' '' '' '' '' '' '              écçÇÅ@ñæç◊ï\              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PRG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
