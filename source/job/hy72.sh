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
../exec/HMY720 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set JRCODE=000;
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '((40,2,N),(1,10,N))' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10),(28,10))' '' '(46,2,N,EQ,@30@)' '' '        出荷集計年間累積Ｆ　集計        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY380 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '(5,4,N)' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10))' '' '(46,2,N,EQ,@30@)' '' '       教育品種別　年間売上集計表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY370 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
