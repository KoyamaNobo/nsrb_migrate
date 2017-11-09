#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                          ＊   '
echo '                 ＊　　工品　仕掛品　棚卸ファイル　クリア　　＊   '
echo '                 ＊                                          ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                  '
echo '                                                                  '
echo '                                                                  '
echo '                       実行する     : 　　CTRL + F5 を押下        '
echo '                     　実行しない　 : 　　CTRL + F9 を押下        '
f5orf9 ; if ($status == 1) exit 1;
FLCNV 'IDE=NO     ODE=MSD    OFI=KHTNF CMD=BOTH CLR=NO'
source ../job/CRC_LIBRARY.sh
ENDJOB:
