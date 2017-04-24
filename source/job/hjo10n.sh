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
../exec/JTO00U $USER_ID $JRCODE 2 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLSF,EFN=JOLSF-TAM;
set ABORT=0;
../exec/JTO10U-TAM $USER_ID $JRCODE 3 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLSF,RESOURCE=RELEASE;
if($JRCODE == 200) then
  goto C
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLSF,EFN=JOLSF-TAM;
set ABORT=0;
../exec/JXO22U-TAM $USER_ID $JRCODE 1 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLSF,RESOURCE=RELEASE;
if($JRCODE == 100) then
  goto C
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLSF,EFN=JOLSF-TAM;
set ABORT=0;
../exec/JTO21U-TAM $USER_ID $JRCODE 1 0000
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLSF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
C:
set ABORT=0;
../exec/JTO00U $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
