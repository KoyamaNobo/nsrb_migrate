#!/bin/tcsh
#***○○○○○○○○　年間一括作表　○○○○○○○○○○○○○○○○○○○○
source ../job/RC_INIT.sh
#***  =====　部門月別　売上対比表　============
#***  --- 得意先年間販売ワーク　集計 ---
#***  INPUT  : TZNTM
#***  OUTPUT : WK0512NNN
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 0 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 年間部門月別　売上対比表 ---
#***  INPUT  : DATEM,TM1,WK0512NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 0 0 0 1 2 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　履物年間　分類販売実績・受払表　============
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HIYF' 'WK0128' '((92,2,N),(96,1,N),(88,2,N),(7,6,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(7,6),(@  @),(1,33))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '    履物年間　分類　販売実績・受払表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 履物年間　分類販売実績・受払表 ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY200 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　品種別　年間製品受払表　============
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HIYF' 'WK0128' '((92,2,N),(96,1,N),(88,2,N),(1,6,N),(7,6,N))' '((1,102),(@          @),(1,16))' '' '((13,6),(19,9),(28,7),(35,10),(45,8),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        品種別　年間製品受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 履物品種別　年間製品受払表 ---
#***  INPUT  : DATEM,HIM1,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 1 0 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 履物品名別　年間製品受払表 ---
#***  INPUT  : DATEM,HIM1,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 1 1 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　得意先年間売上順位別　販売実績表　============
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTMYR' 'WK0128' '((1,4,N),(123,6,N))' '' '' '' '' '' '' '    得意先年間売上集計ファイル　作成    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 得意先年間売上集計ファイル　作成 ---
#***  INPUT  : DATEM,WK0128NNN
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HMY610 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(11,10,S,D)' '' '' '' '' '' '' '   得意先年間売上順位別ファイル　作成   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 得意先年間売上順位別　販売実績表 ---
#***  INPUT  : DATEM,TM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY550 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　担当・地区得意先年間売上順位別　販売実績表　============
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTMYR' 'WK0128' '((1,4,N),(123,6,N))' '' '' '' '' '' '' '    得意先年間売上集計ファイル　作成    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 得意先年間売上集計ファイル　作成 ---
#***  INPUT  : DATEM,WK0128NNN
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HMY610 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((9,2,N),(11,10,S,D))' '' '' '' '' '' '' ' 担当得意先年間売上順位別ファイル　作成 '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先年間売上順位別　販売実績表 ---
#***  INPUT  : DATEM,HKBM,TM1,WK0064NNN
#***  OUTPUT : PRN999
../exec/HMY620 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#***  =====　年間品種別　生産明細表　===========
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SUSRYF' 'WK0064' '((32,1,N),(1,6,N),(46,6,N),(42,1,N))' '((1,51),(@          @),(1,3))' '' '' '' '' '' '          履物年間　生産明細表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 年間品種別　生産明細表 ---
#***  INPUT  : DATEM,HIM1,HKBM,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY750 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　工品年間　用途・品種別　販売実績・製品受払表　===========
#***  --- 工品品名年間累積ファイル　抽出 ---
#***  INPUT  : DATEM,KHM,KHTMYR
#***  OUTPUT : WK0128NNN
set ABORT=0;
../exec/KHY590 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((6,3,N),(1,5,C))' '' '' '' '' '' '' '          工品　年間販売実績表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品年間用途区分別　販売実績表 ---
#***  INPUT  : DATEM,KKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHY540 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品年間用途区分別　製品受払表 ---
#***  INPUT  : DATEM,KKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHY510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品品名年間累積ワーク　変換 ---
#***  INPUT  : WK0128NNN
#***  OUTPUT : WK0256NNN
set ABORT=0;
../exec/KHY520 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品年間用途区分品種別　製品受払表 ---
#***  INPUT  : DATEM,KHM,WK0256NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHY530 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====　担当品種別　売上粗利集計表（ｖｉｖ・教育除く）　===========
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(5,6,N))' '' '' '' '' '' '' '     担当品種別　年間売上粗利集計表     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当品種別　売上粗利集計表 ---
#***  INPUT  : DATEM,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY330 $USER_ID $JRCODE 00 99 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　履物年間得意先品種別　売上集計表　===========
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '(1,10,N)' '' '' '' '' '((11,7,S,NE,@0000000@)O(18,10,S,NE,@0000000000@)O(28,10,S,NE' ',@0000000000@))' '   履物　得意先品種別　年間売上集計表   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 履物年間得意先品種別　売上集計表 ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY310 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　履物・工品　担当得意先月別　売上対比表　=======
#***  --- 得意先年間販売ワーク　集計 ---
#***  INPUT  : TZNTM
#***  OUTPUT : WK0512NNN
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 0 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 履物　担当得意先月別　売上対比表 ---
#***  INPUT  : DATEM,TM1,WK0512NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 工品　担当得意先月別　売上対比表 ---
#***  INPUT  : DATEM,TM1,WK0512NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 0 1 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====　得意先月別　販売実績表　=======
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTMYR' 'WK0128' '((1,4,N),(123,6,N))' '' '' '' '' '' '' '       得意先月別　年間販売実績表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 得意先月別　販売実績表 ---
#***  INPUT  : DATEM,TM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY450 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
