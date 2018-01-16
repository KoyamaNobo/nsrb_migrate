#!/bin/tcsh
source ../job/RC_INIT.sh
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
