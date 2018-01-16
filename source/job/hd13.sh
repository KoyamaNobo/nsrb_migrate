#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=STRAN,EFN=STRANW;
setenv STRAN "STRANW"
set ABORT=0;
../exec/HMD210 $USER_ID $JRCODE 1 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=STRAN,RESOURCE=RELEASE;
unset STRAN
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
