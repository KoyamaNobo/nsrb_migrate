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
#/ASSIGN OEFN=JOLSF,EFN=JOLSF-RYO;
setenv JOLSF "JOLSF-TAM"
set ABORT=0;
../exec/JTO15U-RYO $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLSF,RESOURCE=RELEASE;
unset JOLSF
if($JRCODE == 200) then
  goto C
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLSF,EFN=JOLSF-RYO;
setenv JOLSF "JOLSF-TAM"
set ABORT=0;
../exec/JXO22U-RYO $USER_ID $JRCODE 1 3
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLSF,RESOURCE=RELEASE;
unset JOLSF
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
