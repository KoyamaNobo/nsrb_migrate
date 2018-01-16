#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT745U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '(1,15,C)' '' '' '((16,6),(22,6),(28,6),(34,6),(40,6),(46,6),(52,6),(58,6),(64' ',6),(70,6))' '' '' '      受注数合計表（得意先品名別）      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT750L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
