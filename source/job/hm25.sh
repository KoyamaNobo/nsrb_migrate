#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TCM' 'WK0256' '((1,4,N),(148,14,C),(5,3,N))' '((1,192),(@          @),(1,54))' '' '' '' '(5,3,N,NE,@001@)' '' '    直送先マスター　Ｗチェックリスト    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM230 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
