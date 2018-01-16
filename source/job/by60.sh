#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/KBY200 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,10,N)' '' '' '(11,10)' '' '' '' '     年間　品目区分別　集計表１・２     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBY210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
