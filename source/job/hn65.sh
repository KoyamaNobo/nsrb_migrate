#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0512' '((1,6,N),(13,1,N))' '((1,256),(@          @),(1,246))' '' '' '' '' '' '   �i���T�C�Y�ʒI���덷�t�@�C���@�쐬   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN420 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((80,2,N),(84,1,N),(76,3,N),(1,7,N),(85,1,N))' '' '' '' '' '' '' '        �i���T�C�Y��  �I���덷�\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN430 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
