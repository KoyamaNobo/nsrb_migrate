#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=OKJF,EFN=OKJF-RYO;
set ABORT=0;
../exec/JT053R-RYO $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=OKJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
