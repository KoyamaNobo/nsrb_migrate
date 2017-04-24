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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((28,2,N),(1,10,N))' '((1,42),(@          @),(1,12))' '' '' '' '(36,1,N,EQ,@1@)' '' '    担当得意先品種別　年間返品明細表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY810 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((34,2,N),(5,4,N),(37,6,N))' '((1,42),(@          @),(1,12))' '' '((11,7),(18,10))' '' '(36,1,N,EQ,@1@)' '' '         品種別　年間返品明細表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY820 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HPYRF' 'WK0064' '((34,2,N),(5,6,N),(37,6,N))' '((1,42),(@          @),(1,12))' '' '((11,7),(18,10))' '' '(36,1,N,EQ,@2@)' '' '         品種別　不良返品明細表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY850 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
