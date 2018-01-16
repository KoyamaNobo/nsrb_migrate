#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                   ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                   ＊　　　ＭＳＤセーブ後　評価替え処理　　　　＊   '
echo '                   ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                   ＨＩＭ　振替単価変更                             '
echo '                   ＨＵＨＭ　前残・翌繰金額変更                     '
echo '                   各累積ファイル　振替金額再セット                 '
echo '                                                                    '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
source ../job/db_bkup.sh "hm83.sh" "\!"
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMM770 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
