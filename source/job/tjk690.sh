#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=JSTR,EFN=JSTR-TAM;
setenv JSTR "JSTR-TAM"
set ABORT=0;
../exec/JK069I $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JSTR,RESOURCE=RELEASE;
unset JSTR
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
