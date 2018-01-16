#!/bin/tcsh
source ../job/RC_INIT.sh
A:
set JRCODE=000;
set ABORT=0;
../exec/PR410U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto B
endif
set ABORT=0;
../exec/PR415L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
B:
set JRCODE=000;
set ABORT=0;
../exec/PR420U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto C
endif
set ABORT=0;
../exec/PR425L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
C:
set JRCODE=000;
set ABORT=0;
../exec/PR430U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto D
endif
set ABORT=0;
../exec/PR435L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
D:
set JRCODE=000;
set ABORT=0;
../exec/PR510U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto E
endif
set ABORT=0;
../exec/PR515L $USER_ID $JRCODE 
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
E:
set JRCODE=000;
set ABORT=0;
../exec/PR520U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/PR525L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
