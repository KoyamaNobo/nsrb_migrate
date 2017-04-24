#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊　　　　　ＪＡＮコード　再セット　　　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
if (`echo "$<" | grep F9 | wc -l ` == 1) exit
#**
set ABORT=0;
NFCNV 'MN1=C MN2=DA PA1=MSD PA3=WK0064008 PA4=1 \
PA5=N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N6,N1,N2,C10,C32 \
PB1=../tmp/jan.csv PB2=CSV1 PB3=PROTECT PB4=COMMA \
PB7=S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,C,C PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/JAN01U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
