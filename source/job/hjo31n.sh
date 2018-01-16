#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTO00U $USER_ID $JRCODE 1 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLJF,EFN=JOLJF-RYO;
set ABORT=0;
../exec/JXO30U-RYO $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=JOLJF,EFN=JOLJF-RYO;
set ABORT=0;
../exec/JTO31U-RYO $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JOLJF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTO00U $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
