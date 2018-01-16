#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=TDI-RDB,EFN=TDIF-TAM;
set ABORT=0;
../exec/JTN08R-TAM $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDI-RDB,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
