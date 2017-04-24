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
../exec/PRD350 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKG970 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKG970 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBD110 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD120 $USER_ID $JRCODE 1 1
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0064' '((54,8,N),(1,4,N))' '((54,8),(1,4),(186,1),(@          @),(1,41))' '' '(186,1)' '' '((54,8,N,NE,@00000000@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@' '000000@))' '        êøãÅèëÅ@î≠çsó\íËÅ@ñ‚çáÇπ        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG830 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
