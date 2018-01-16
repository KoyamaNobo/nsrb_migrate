#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'JSJDRF' 'SHA' 'WK0256' 'CRE' '' '' '' '' '' '' '         出荷指図書　再発行　           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTO34L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
