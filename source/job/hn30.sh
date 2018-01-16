#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMN510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((28,2,N),(32,1,N),(24,3,N),(1,6,N),(19,5,N))' '' '' '' '' '' '' '            óöï®Å@íIâµåÎç∑ï\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN620 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/HMN610 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
