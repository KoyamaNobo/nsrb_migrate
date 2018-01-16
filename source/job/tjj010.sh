#!/bin/tcsh
source ../job/RC_INIT.sh
#**
#/ASSIGN OEFN=OKJF,EFN=OKJF-TAM;
set ABORT=0;
../exec/JTN10U-TAM $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=OKJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../job/fuku01.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/JTN11U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
