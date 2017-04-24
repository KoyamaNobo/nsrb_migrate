#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#/ASSIGN OEFN=JNIF,EFN=JNIF-TAM;                                            00020
set ABORT=0;
../exec/JV010U-TAM $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JNIF,RESOURCE=RELEASE;  
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
SORT 'SRT= IDE=MSD ICI= IFI=NIFUDAW ODE=MSD OCI= OFI=NIFUDAW WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((61,6,N),(511,9,N),(1,7,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/ASSIGN OEFN=JNIF,EFN=JNIF-TAM;
set ABORT=0;
../exec/JV100U-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JNIF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../job/e-nifu.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
