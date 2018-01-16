#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(1,10,N),(53,6,N))' '' '' '((11,7),(18,10),(28,10))' '' '' '' '    担当得意先品種別　年間売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY360 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
