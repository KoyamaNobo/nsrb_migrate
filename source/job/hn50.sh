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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((47,2,N),(51,1,N),(43,3,N),(1,4,N),(29,5,N),(15,5,N))' '' '' '((8,7),(20,9),(34,9))' '' '' '' '      óöï®Å@åàéZópíIâµç∑äzÅ@ñæç◊ï\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMN950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
