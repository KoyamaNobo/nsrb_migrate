#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#*** ＊＊＊＊＊＊＊＊　 東京（ヴィヴェンディ） 　＊＊＊＊＊＊＊＊＊＊＊＊＊
#***  =====  品名サイズ別　受払表（ヴィヴェンディ）  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTYR' 'WK0256' '((251,6,N),(248,2,N),(250,1,N),(244,3,N),(7,6,N))' '' '' '' '' '(244,3,N,EQ,@322@)' '' '          品名サイズ別　受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 品名サイズ別 受払表 ---
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
