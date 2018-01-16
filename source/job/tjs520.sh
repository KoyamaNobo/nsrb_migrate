#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=TDNNF,EFN=TDNNF-TAM;
set ABORT=0;
../exec/JHS52L-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDNNF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
