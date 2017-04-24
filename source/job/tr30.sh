#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                                                                   '
echo '                                                                   '
echo '                     【　　領　収　書　　】                        '
set ABORT=0;
SORT 'SRT= IDE=MSD ICI= IFI=RSF ODE=MSD OCI= OFI=RSF \
WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((13,10,N),(37,2,N),(62,3,N)) \
OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/TSR100 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSR150 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
