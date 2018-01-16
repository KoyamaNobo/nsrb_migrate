#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PRD350 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((1,8,N),(9,4,N),(51,8,C))' '' '' '' '' '' '' '           入金ファイル　生成           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PRD360 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
