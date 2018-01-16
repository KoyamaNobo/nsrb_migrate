#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMN900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '((47,2,N),(51,1,N),(43,3,N))' '((1,64),(@          @),(1,54))' '' '((8,7),(20,9),(34,9))' '' '' '' '      分類　決算用棚卸差額　集計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMN960 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
