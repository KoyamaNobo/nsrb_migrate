#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0
../exec/TSR030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TNKF' 'WK0064' '((1,10,N),(20,2,N))' '((1,32),(@          @),(1,22))' '' '(11,9)' '' '(7,4,N,NE,@4745@)' '' '      領収書チェックファイル　集計      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'TNKF' 'SHA' 'WK0064' 'ADD' '' '' '' '' '(7,4,N,EQ,@4745@)' '' '      領収書チェックファイル　変換      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSR010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'RSF' 'WK0064' '((13,10,N),(37,2,N),(62,3,N))' '' '' '' '' '' '' '         領収書　チェックリスト         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSR020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
