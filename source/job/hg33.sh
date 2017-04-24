#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'STRANYR' 'WK0128' '(1,7,N)' '' '' '' '' '((8,6,N,GE,@201504@)A(8,6,N,LE,@201603@)A(7,1,N,LE,@6@)A(128' ',1,N,EQ,@0@))' '      履物　品名別　不良返品合計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((89,2,N),(125,1,N),(85,2,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '((57,5),(67,8))' '' '((20,4,N,NE,@9999@)A(76,1,N,EQ,@2@))' '' '      履物　品名別　不良返品合計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG375 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((48,2,N),(58,1,N),(44,2,N),(60,1,N),(13,6,N))' '' '' '' '' '' '' '      履物　品名別　不良返品合計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG385 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../job/e-fury.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊　　　　￥ＨＥＮＫＡＮ￥ＨＦＵＲＹＯ．ＣＳＶ＊   '
echo '                 ＊        不良返品分                          ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
if (`echo "$<" | grep F9 | wc -l ` == 1) exit
ENDJOB:
