#!/bin/tcsh
source ../job/RC_INIT.sh
#**
#/ASSIGN OEFN=OKJF,EFN=OKJF-RYO;
#/ASSIGN OEFN=FUKUF,EFN=FUKUF1;
set ABORT=0;
../exec/JTN10U-RYO $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=OKJF,RESOURCE=RELEASE;
#/ASSIGN OEFN=FUKUF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0
../job/fuku11.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/ASSIGN OEFN=FUKUF,EFN=FUKUF1;
set ABORT=0;
../exec/JTN11U-RYO $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=FUKUF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
