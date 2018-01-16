#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KHTMYR' 'WK0064' '(165,6,N)' '((165,6),(1,8),(49,8),(57,7),(@          @),(1,25))' '' '' '' '((49,8,S,NE,@00000000@)O(57,7,S,NE,@0000000@))' '' '          工品　年間製品受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/KHY750 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
