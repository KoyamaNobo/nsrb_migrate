#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=JSTR,EFN=JSTR-TAM;
set ABORT=0;
../exec/JT051R-TAM $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JSTR,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
