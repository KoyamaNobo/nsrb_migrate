#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
../exec/JT400U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 130) then
  goto A
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN40U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSMSF' 'WK0128' '((24,1,N),(17,7,N),(1,7,N))' '' '' '' '' '((110,1,N,EQ,@0@)A(126,1,N,EQ,@1@))' '' '           履物売上伝票　変換           ''10' 'HSMSF' 'WK0128' '((24,1,N),(17,7,N),(1,7,N))' '' '' '' '' '((110,1,N,EQ,@0@)A(126,1,N,EQ,@1@))' '' '           履物売上伝票　変換           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JT110L $USER_ID $JRCODE 4
source ../job/CRC_LIBRARY.sh
if($JRCODE == 120) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
A:
set ABORT=0;
../exec/JTN45U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((40,6,N),(25,4,N),(46,1,N))' '' '' '((92,4),(96,4),(100,4),(104,4),(108,4),(112,4),(116,4),(120,' '4),(124,4),(128,4))' '' '' '   　　　　　品名別出荷日報 　　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT420L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
