#!/bin/tcsh
source ../job/RC_INIT.sh
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
