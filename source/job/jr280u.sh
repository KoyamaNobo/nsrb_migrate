#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PR280U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/PR285U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
