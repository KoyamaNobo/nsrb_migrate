#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
../exec/JTN00U $USER_ID $JRCODE 6 1 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JSTR,EFN=JSTR-TAM;
#/ASSIGN OEFN=JOLJF,EFN=JOLJF-TAM;
set ABORT=0;
../exec/JKN72U-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JSTR,RESOURCE=RELEASE;
#/ASSIGN OEFN=JOLJF,RESOURCE=RELEASE;
if($JRCODE == 100) then
  goto E
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/ASSIGN OEFN=JSTR,EFN=JSTR-TAM;
#/ASSIGN OEFN=JOLJF,EFN=JOLJF-TAM;
set ABORT=0;
../exec/JK073U-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JSTR,RESOURCE=RELEASE;
#/ASSIGN OEFN=JOLJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
E:
set ABORT=0;
../exec/JTN00U $USER_ID $JRCODE 6 0 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
