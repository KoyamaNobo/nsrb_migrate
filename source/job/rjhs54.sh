#!/bin/tcsh
source ../job/RC_INIT.sh
#/ASSIGN OEFN=TDNAF,EFN=TDNAF-RYO;
set ABORT=0;
../exec/JHS54L-RYO $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDNAF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
