#!/bin/tcsh
source ../job/RC_INIT.sh
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
