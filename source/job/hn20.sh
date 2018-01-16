#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMN450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((8,8,N),(7,1,N))' '' '' '((16,6),(22,6),(28,6),(34,6),(40,6),(46,6),(52,6),(58,6),(64' ',6),(70,6))' '' '' '            倉別　棚卸明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((97,1,N),(91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            倉別　棚卸明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD620 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
ENDJOB:
