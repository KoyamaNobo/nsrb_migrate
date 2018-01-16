#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTN00U $USER_ID $JRCODE 5 2 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN06U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 200) then
  goto C
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((1,46,C),(92,56,C))' '' '' '((47,4),(51,4),(55,4),(59,4),(63,4),(67,4),(71,4),(75,4),(79' ',4),(83,4),(87,5))' '(40,6,N,NE,@999999@)' '' '    トラスコ他指図変換ワーク作成      　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((1,19,C),(37,3,N),(92,56,C),(20,10,C),(40,7,N))' '' '' '' '' '' '' '    トラスコ他指図変換ワーク作成      　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN07U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 200) then
  goto C
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN35L $USER_ID $JRCODE 6
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
C:
set ABORT=0;
../exec/JTN00U $USER_ID $JRCODE 5 0 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
