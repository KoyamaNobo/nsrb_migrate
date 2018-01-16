#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=STRANYR,EFN=BB-STRAN;
setenv STRANYR "BB-STRAN"
set ABORT=0;
../exec/HMD220 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=STRANYR,RESOURCE=RELEASE;
unset STRANYR
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
