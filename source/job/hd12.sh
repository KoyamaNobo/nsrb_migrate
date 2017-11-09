#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#/ASSIGN OEFN=STRAN,EFN=STRANW;
setenv STRAN "STRANW"
set ABORT=0;
../exec/HMD210 $USER_ID $JRCODE 1 0
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=STRAN,RESOURCE=RELEASE;
unset STRAN
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
