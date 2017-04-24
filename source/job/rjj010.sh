#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
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
