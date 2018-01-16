#!/bin/tcsh
source ../job/RC_INIT.sh
A:
set ABORT=0;
../exec/HMY240 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
if ($JRCODE == 130) then
  goto C
endif
if ($JRCODE == 150) then
  goto C
endif
B:
set ABORT=0;
set NORMAL=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(1,10,N))' '' '' '' '' '' '' '   得意先品種月別　年間売上数量集計表   '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($NORMAL == 1) then
  goto D
endif
C:
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '(1,10,N)' '' '' '' '' '' '' '   得意先品種月別　年間売上数量集計表   '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
D:
set ABORT=0;
../exec/HMY250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
