#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊　　　　決算廃却未決定時他　伝票処理        ＊   '
echo '                 ＊　　　　　ＢＢ－ＳＴＲＡＮ　クリア　　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
echo '                                                                    '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
FLCNV 'IDE=NO ODE=MSD OCI= OFI=BB-STRAN OGN= CMD=BOTH CLR=NO SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
