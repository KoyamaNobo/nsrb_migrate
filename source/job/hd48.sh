#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMG810 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((7,6,N),(13,1,N))' '' '' '(16,1)' '' '' '' '                                        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 5
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
set JRCODE=000;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((98,6,N),(104,1,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            óöï®Å@ç›å…ñæç◊ï\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD620 $USER_ID $JRCODE 1 000000 999999 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
