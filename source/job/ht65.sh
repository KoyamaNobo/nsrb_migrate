#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'STRAN-3' 'WK0128' '(1,7,N)' '' '' '' '' '' '' '       履物売上・値引伝票　問合せ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'SNTRF' 'SHA' 'WK0128' 'ADD' '' '' '' '' '' '' '       履物売上・値引伝票　問合せ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT650 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
