#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT800U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((149,1,N),(147,1,N),(1,7,N),(8,1,N),(9,7,N))' '' '' '' '' '' '' '   　　　　出荷指図残リスト 　　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT110L $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
ENDJOB:
