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
