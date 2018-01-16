#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PR441L $USER_ID $JRCODE 6607 6607 6612 6632 6634 7118 7120 7123 7126 7127 7129 8200 8202 9999
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/PR445L $USER_ID $JRCODE 66010000 99990000  
source ../job/CRC_LIBRARY.sh
ENDJOB:
