#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '(92,2,N)' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(@' '         @))' '' '' '(7,1,N,NE,@9@)' '' '  履物部門担当得意先別　売上集計問合せ  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT360 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
