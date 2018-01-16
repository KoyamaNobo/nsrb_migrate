#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HKG240 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TSKF' 'WK0256' '((185,1,N),(188,1,N),(1,4,N))' '' '' '' '' '(169,8,N,NE,@00000000@)' '' '               êøãÅñæç◊èë               '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKG250 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
