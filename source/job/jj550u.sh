#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT550U $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto A
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((9,8,N),(1,6,N),(7,1,N))' '' '' '' '' '' '' '�@�@�@�@�@�o�׊m�薢�������X�g�@�@�@�@�@'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT560L $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
A:
set JRCODE=000;
set ABORT=0;
../exec/JT550U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((9,8,N),(1,6,N),(7,1,N))' '' '' '' '' '' '' '�@�@�@�@�@����󖢍X�V���X�g�@�@�@�@�@�@'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT560L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
