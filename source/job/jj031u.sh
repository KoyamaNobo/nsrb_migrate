#!/bin/tcsh
source ../job/RC_INIT.sh
STEP1:
set ABORT=0;
../exec/JT031U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
STEP2:
set ABORT=0;
../exec/JT032U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
STEP3:
set ABORT=0;
../exec/JT033U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
STEP4:
set ABORT=0;
../exec/JT034U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
STEP7:
set ABORT=0;
../exec/JT038U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
STEP8:
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((149,1,N),(147,1,N),(1,7,N),(8,1,N),(9,7,N))' '' '' '' '' '' '' '   　　　　　出荷指図リスト 　　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
STEP9:
set ABORT=0;
../exec/JT110L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
STEP10:
set ABORT=0;
../exec/JT036U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
STEP11:
../exec/JT037U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
