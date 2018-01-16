#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '(1,10,N)' '' '' '' '' '((11,7,S,NE,@0000000@)O(18,10,S,NE,@0000000000@)O(28,10,S,NE' ',@0000000000@))' '   履物　得意先品種別　年間売上集計表   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
