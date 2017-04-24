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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        分類　集計ファイル　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
