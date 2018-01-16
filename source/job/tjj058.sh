#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=OKJF,EFN=OKJF-TAM;
set ABORT=0;
../exec/JT058L-TAM $USER_ID $JRCODE 2 0
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=OKJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
