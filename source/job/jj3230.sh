#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT321U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((2,6,N),(1,1,N),(82,1,N),(81,1,N))' '' '' '((9,6),(15,6),(21,6),(27,6),(33,6),(39,6),(45,6),(51,6),(57,' '6),(63,6))' '' '' '        品種別　在庫受払表  　　        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT323L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
