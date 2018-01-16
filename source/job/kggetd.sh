#!/bin/tcsh
source ../job/RC_INIT.sh
#*** �����������������@ �H      �i �@�������������������������������������� 
#***  =====  �H�i�i����  �̔����ѕ\(����)  ======== 
#***  INPUT  : DATEM,KHM,KHTM2 
#***  OUTPUT : PRN999 
set ABORT=0;
../exec/KHG020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�a����@�󕥕\  ======== 
#***  INPUT  : DATEM,KHM,KHTM2 
#***  OUTPUT : PRN999 
set ABORT=0;
../exec/KHG250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �����p�p���ו\  ======== 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((47,2,N),(11,5,C))' '' '' '' '' '(1,2,N,LE,@28@)' '' '           �����E�p�p�@���ו\           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- �����p�p���ו\ --- 
#***  INPUT  : DATEM,KHM,WK0064NNN 
#***  OUTPUT : PRN999 
set ABORT=0;
../exec/KHG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �s�Ǔ��v�\  ======== 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((11,5,C),(1,2,N))' '' '' '' '' '((1,2,N,GE,@01@)A(1,2,N,LE,@28@))' '' '        �@     �s�ǖ��ו\               '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- �s�Ǔ��v�\ --- 
#***  INPUT  : DATEM,KHM,WK0064NNN 
#***  OUTPUT : PRN999 
set ABORT=0;
../exec/KHG350 $USER_ID $JRCODE 0 '     ' 99999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
