#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TDIF-TAM' 'WK0256' '((8,6,N),(14,7,N),(21,4,N),(1,7,N))' '' '' '' '' '' '' '    ìùàÍì`ï[î≠çsÅiÉgÉâÉXÉRëºÇeÇ`ÇwÅj    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/ASSIGN OEFN=TDIF,EFN=TDIF-TAM;
set ABORT=0;
../exec/JHS70L-TAM $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
#/ASSIGN OEFN=TDIF,RESOURCE=RELEASE;
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
