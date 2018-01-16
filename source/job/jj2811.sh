#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT283U $USER_ID $JRCODE 3 9 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((1,8,N),(86,1,C))' '' '' '((9,6),(15,6),(21,6),(27,6),(33,6),(39,6),(45,6),(51,6),(57,' '6),(63,6))' '' '' '          ëqï ç›å…Å@ñæç◊ï\              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT281L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
