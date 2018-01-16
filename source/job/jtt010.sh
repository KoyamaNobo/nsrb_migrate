#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTT01U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((69,13,C),(2,6,N),(40,4,C))' '' '' '' '' '' '' '                                        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTT02U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#***                                 WK0128 Å®  \HENKAN\TZAIW.CSV
../job/e-tamz.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
