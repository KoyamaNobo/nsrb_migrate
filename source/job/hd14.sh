#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=STRAN,EFN=STRANW;
setenv STRAN "STRANW"
set ABORT=0;
../exec/HMD220 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=STRAN,RESOURCE=RELEASE;
unset STRAN
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD040 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=STRANW IGN= IFO=PROTECT ODE=MSD OCI= OFI=BB-STRAN OGN= LST=NO ERR=ABORT MOD=ADD UMD=NO GTR= DEL=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=NO ODE=MSD OCI= OFI=STRANW OGN= CMD=BOTH CLR=NO SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
