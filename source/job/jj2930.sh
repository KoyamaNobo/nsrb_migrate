#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT293U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '(16,4,N)' '' '' '' '' '' '' '      ìæà”êÊï éÛíçëºÅ@ã‡äzñæç◊ï\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT277L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
