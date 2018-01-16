#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT285U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((10,6,N),(249,2,N),(16,4,N),(42,3,N),(34,7,N))' '' '' '' '' '' '' '    受注残帳（品名・担当・得意先） 白紙 '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT287L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
