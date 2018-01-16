#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../job/e-ttan.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/JV710U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((7,1,N),(1,6,C),(8,6,C),(14,6,N),(81,1,N))' '' '' '((21,6),(27,6),(33,6),(39,6),(45,6),(51,6),(57,6),(63,6),(69' ',6),(75,6))' '' '' '                                        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/JV610U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
