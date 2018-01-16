#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((11,5,C),(1,2,N))' '' '' '' '' '((1,2,N,GE,@01@)A(1,2,N,LE,@28@))' '' '        Å@ñhêUÉSÉÄÅ@ïsó«ñæç◊ï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG350 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
