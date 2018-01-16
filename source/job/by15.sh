#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JTIF' 'WK0064' '((18,2,N),(1,6,N))' '((1,21),(1,21),(1,21),(1,1))' '' '(9,9)' '' '' '' '           çﬁóøÅ@íIâµÅ@ñæç◊ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG570 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
