#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#/ASSIGN OEFN=JSTR,EFN=JSTR-TAM;
#/ASSIGN OEFN=JNIF,EFN=JNIF-TAM;
#/ASSIGN OEFN=OKJF,EFN=OKJF-TAM;
#/ASSIGN OEFN=TDNWF,EFN=TDNWF-TAM;
#/ASSIGN OEFN=TDNNF,EFN=TDNNF-TAM;
#/ASSIGN OEFN=TDI-RDB,EFN=TDIF-TAM;
#/ASSIGN OEFN=TDNAF,EFN=TDNAF-TAM;
set ABORT=0;
../exec/JK090U-TAM $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=JSTR,RESOURCE=RELEASE;
#/ASSIGN OEFN=JNIF,RESOURCE=RELEASE;
#/ASSIGN OEFN=OKJF,RESOURCE=RELEASE;
#/ASSIGN OEFN=TDNWF,RESOURCE=RELEASE;
#/ASSIGN OEFN=TDNNF,RESOURCE=RELEASE;
#/ASSIGN OEFN=TDI-RDB,RESOURCE=RELEASE;
#/ASSIGN OEFN=TDNAF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
