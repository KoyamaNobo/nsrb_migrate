#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=TDNWF,EFN=TDNWF-TAM;
set ABORT=0;
../exec/JHS51L-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDNWF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
