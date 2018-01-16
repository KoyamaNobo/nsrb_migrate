#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PRG110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PRG150 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,4,N),(15,1,N))' '' '' '(5,10)' '' '' '' '         経費　相手科目　内訳表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PRG160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
