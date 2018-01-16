#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=JNIF,EFN=JNIF-TAM;
set ABORT=0;
../exec/JT055R-TAM $USER_ID $JRCODE 9 6
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JNIF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
