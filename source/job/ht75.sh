#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CFLC20 $USER_ID $JRCODE '01' 'SSRYF' 'WK0064' '' '' '' '' '' '' '        品名月別前年対比ワーク作成       '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMT830 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((5,6,N),(53,6,N))' '' '' '((11,7),(18,10))' '' '' '' '        品名月別前年対比ワーク作成       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMT820 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT810 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
