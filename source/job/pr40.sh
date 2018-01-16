#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PRG410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((1,8,N),(501,1,N))' '' '' '' '' '' '' '         â»ñ⁄åéï Å@è¡îÔê≈ì‡ñÛï\         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PRG460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
