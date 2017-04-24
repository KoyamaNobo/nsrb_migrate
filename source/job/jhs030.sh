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
../exec/JHS01U $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/RUN $FBFT,DEV=MSD;
#MSD_RECV21_NO _NO _YES_
#/> ;
#/: ABORT          JUMP=J;
#/: JRCODE EQ 000  JUMP=C;
#/: NORMAL JUMP=E;
C:
set ABORT=0;
../exec/JHS15U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS25L $USER_ID $JRCODE 0 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
E:
#/RUN $FBFT,DEV=MSD;
#MSD_RECV22_NO _NO _YES_
#/> ;
#/: ABORT          JUMP=J;
#/: JRCODE EQ 000  JUMP=F;
#/: NORMAL JUMP=J;
F:
set ABORT=0;
../exec/JHS35U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS37L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
J:
set ABORT=0;
../exec/JHS31U $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
