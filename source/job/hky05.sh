#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CFLC20 $USER_ID $JRCODE '01' 'TUKOYR' 'WK0064' '' '' '' '' '' '' '           �s�t�j�n�x�e�@�ϊ�           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'TUKF1' 'SHA' 'WK0064' 'ADD' '' '' '' '' '' '' '           �s�t�j�e�P�@�ϊ�             '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '((19,8,N),(1,4,N))' '((1,64),(@          @),(1,54))' '' '' '' '' '' '       ���Ӑ挳���������[�N�@�쐬       '
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((34,6,N),(33,1,N),(1,4,N),(5,8,N),(29,1,N))' '' '' '((13,9),(22,7))' '' '' '' '           ���Ӑ挳���@�����\           '
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((1,1,N),(18,4,N),(10,8,N),(2,8,N),(222,6,N),(54,1,C))' '' '' '' '' '' '' '           ���@�Ӂ@��@��@��             '
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
