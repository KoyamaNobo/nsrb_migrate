#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TAZM' 'WK0064' '((5,6,N),(1,4,N))' '((1,42),(@          @),(1,12))' '' '' '' '' '' '        ïiéÌï Å@ìæà”êÊóaÇËñ‚çáÇπ        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
