#!/bin/tcsh
source ../job/RC_INIT.sh
#*** �����������������@ �����i���B���F���f�B�j �@��������������������������
#***  =====  �i���T�C�Y�ʁ@�󕥕\�i���B���F���f�B�j  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTYR' 'WK0256' '((251,6,N),(248,2,N),(250,1,N),(244,3,N),(7,6,N))' '' '' '' '' '(244,3,N,EQ,@322@)' '' '          �i���T�C�Y�ʁ@�󕥕\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �i���T�C�Y�� �󕥕\ ---
#***  INPUT  : DATEM,HIM1,WK0256
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG720 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
