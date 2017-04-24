#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#*** ＊＊＊＊＊＊＊＊　 工      品 　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊ 
#***  =====  工品品名別  販売実績表(原価)  ======== 
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
#***  =====  工品預かり　受払表  ======== 
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
#***  =====  加硫廃却明細表  ======== 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((47,2,N),(11,5,C))' '' '' '' '' '(1,2,N,LE,@28@)' '' '           加硫・廃却　明細表           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 加硫廃却明細表 --- 
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
#***  =====  不良統計表  ======== 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((11,5,C),(1,2,N))' '' '' '' '' '((1,2,N,GE,@01@)A(1,2,N,LE,@28@))' '' '        　     不良明細表               '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 不良統計表 --- 
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
