#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((28,2,N),(1,10,N))' '((1,42),(@          @),(1,12))' '' '' '' '(36,1,N,EQ,@1@)' '' '    �S�����Ӑ�i��ʁ@�N�ԕԕi���ו\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY810 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((34,2,N),(5,4,N),(37,6,N))' '((1,42),(@          @),(1,12))' '' '((11,7),(18,10))' '' '(36,1,N,EQ,@1@)' '' '         �i��ʁ@�N�ԕԕi���ו\         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY820 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((34,2,N),(5,6,N),(37,6,N))' '((1,42),(@          @),(1,12))' '' '((11,7),(18,10))' '' '(36,1,N,EQ,@2@)' '' '         �i��ʁ@�s�Ǖԕi���ו\         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY850 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
