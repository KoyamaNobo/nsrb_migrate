#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/KBY100 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,4,N)' '' '' '(5,10)' '' '' '' '       �����ʁ@�N�Ԏd�������ו\       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(5,10,S,D)' '' '' '' '' '' '' '       �����ʁ@�N�Ԏd�������ו\       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBY110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
