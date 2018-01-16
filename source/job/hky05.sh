#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CFLC20 $USER_ID $JRCODE '01' 'TUKOYR' 'WK0064' '' '' '' '' '' '' '           ÇsÇtÇjÇnÇxÇeÅ@ïœä∑           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'TUKF1' 'SHA' 'WK0064' 'ADD' '' '' '' '' '' '' '           ÇsÇtÇjÇeÇPÅ@ïœä∑             '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '((19,8,N),(1,4,N))' '((1,64),(@          @),(1,54))' '' '' '' '' '' '       ìæà”êÊå≥í†ëçäáÉèÅ[ÉNÅ@çÏê¨       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKG720 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((34,6,N),(33,1,N),(1,4,N),(5,8,N),(29,1,N))' '' '' '((13,9),(22,7))' '' '' '' '           ìæà”êÊå≥í†Å@ëçäáï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKY010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((1,1,N),(18,4,N),(10,8,N),(2,8,N),(222,6,N),(54,1,C))' '' '' '' '' '' '' '           ìæÅ@à”Å@êÊÅ@ë‰Å@í†             '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKY050 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
