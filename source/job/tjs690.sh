#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=TDI-RDB,EFN=TDIF-TAM;
set ABORT=0;
../exec/JHS69L-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDI-RDB,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
