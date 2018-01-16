#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '((15,6,C),(59,6,C),(74,1,N),(11,4,N),(1,2,N),(53,4,C))' '((1,102),(@          @),(1,16))' '' '((21,9),(38,8))' '' '((15,6,C,NE,@999000@)A(16,1,C,EQ,@9@))' '' '       仕入支払累積ファイル　集計       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((41,2,N),(33,6,N,D),(1,10,N),(27,6,N))' '' '' '' '' '' '' '            製品仕入　明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG620 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '((15,6,C),(59,6,C),(74,1,N),(11,4,N),(1,2,N),(53,4,C))' '((1,102),(@          @),(1,16))' '' '((21,9),(38,8))' '' '((15,6,C,NE,@999000@)A(16,1,C,EQ,@9@))' '       仕入支払累積ファイル　集計       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((41,2,N),(33,6,N,D),(1,6,N))' '' '' '((11,6),(17,9))' '' '' '' '            製品仕入　明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG630 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
