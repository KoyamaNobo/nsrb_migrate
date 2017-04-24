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
../exec/HMD310 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HN290U $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
FLCNV 'IDE=MSD000 ICI= IFI=UTRAN IGN= IFO=SHARE \
ODE=MSD000 OCI= OFI=UTRYR OGN= LST=NO ERR=ABORT MOD=ADD UMD=NO GTR= \
DEL=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
ENDJOB:
