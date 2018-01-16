#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT025U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((8,1,N),(1,6,N),(170,3,N),(7,1,N))' '' '' '' '' '' '' '         éÛíçÅEóaÇËÅEéÊÇÊÇØì˙ïÒ         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT020L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
