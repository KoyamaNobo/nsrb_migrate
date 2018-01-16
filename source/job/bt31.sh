#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRYR' 'WK0128' '(2,8,N)' '((1,102),(@          @),(@          @),(@      @))' '' '' '' '(1,2,N,NE,@30@)' '' '       仕入先日付別仕入明細問合せ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'JSSRF' 'SHA' 'WK0128' 'ADD' '' '' '' '' '' '' '       仕入先日付別仕入明細問合せ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((11,4,N),(3,8,N),(95,7,N))' '' '' '' '' '' '' '       仕入先日付別仕入明細問合せ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0
../exec/KBT310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
