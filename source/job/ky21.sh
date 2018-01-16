#!/bin/tcsh
source ../job/RC_INIT.sh
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
