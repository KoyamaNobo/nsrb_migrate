#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#***○○○○○○○○　 事務所・工品他　連続用紙  　○○○○○○○○○○○○
#*** ＊＊＊＊＊＊＊＊　 事  務  所 　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
#***  =====  履物表紙  ========
#***  INPUT  : DATEM
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG000 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  部門別日計集計表  =====
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,URIRF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG400 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  履物日計表  =====
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,UTRF,STRAN,NYUF,UTRAN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMD710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  工品他日計表  =====
#***  INPUT  : DATEM,TTM,CALNM,URIRF,NYURF,BUMON-K,URIF,NYUF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHD610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  部門月別売上対比表  ==========
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TZNTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=TZNTPM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 得意先年間販売ワーク 更新 ---
#***  INPUT  : DATEM,TTM,SNTRF
#***  I-O    : TZNTPM
set ABORT=0;
../exec/HMG990 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 得意先年間販売ワーク 集計 ---
#***  INPUT  : TZNTPM
#***  OUTPUT : WK0512NNN
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 部門月別 売上対比表 ---
#***  INPUT  : DATEM,TM1,WK0512NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 0 0 1 2 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品用途区分別 販売実績表  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品用途区分別 製品受払表  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
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
#***  =====  工品マット他 販売実績表  =====
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG040 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
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
#***  =====  工品他 得意先品名別 売上集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 工品他 得意先品名別 売上集計表 ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品品名別  販売実績受払表(参考)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品他 得意先品名別 売上集計表（参考）  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- 工品他 得意先品名別 売上集計表（参考） ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  分類販売実績表・製品受払表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        分類　集計ファイル　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 分類販売実績表 ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 分類製品受払表 ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  分類  品種・品名別　製品受払表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           履物　品種別受払表           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 分類品種別製品受払表 ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 0 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 分類品名別製品受払表 ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 1 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  分類品名別　在庫明細表  =====
#***  --- 在庫表ワーク 作成 ---
#***  INPUT  : HHTF1,HIM1
#***  OUTPUT : WK0128NNN
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            履物　在庫明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 分類品名別在庫明細表 ---
#***  INPUT  : DATEM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
../exec/HMD620 $USER_ID $JRCODE 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
#***  =====  生産関係　作表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UTRF' 'WK0128' '((85,1,N),(16,4,N),(84,1,N))' '' '' '' '' '' '' '            履物　生産明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 生産区分品名別 生産明細表 ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  履物品名別不良返品合計表　作表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0128' '(1,7,N)' '' '' '' '' '((7,1,N,LE,@6@)A(128,1,N,EQ,@0@))' '' '      履物　品名別　不良返品合計表      '
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
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((48,2,N),(58,1,N),(44,2,N),(60,1,N),(13,6,N))' '' '' '' '' '' '' '      履物　品名別　不良返品合計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG380 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  履物品名別格外品合計表　作表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UTRF' 'WK0064' '(16,6,N)' '((8,8),(@0000@),(16,6),(63,5),(@00000@),(@00000000@),(@00000' '00@),(88,6),(@0000@),(@0000000@),(@    @))' '(63,5)' '' '((16,4,N,LE,@9999@)A(84,1,N,EQ,@5@))' '' '       履物　品名別　格外品合計表       '
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
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((48,2,N),(58,1,N),(44,2,N),(60,1,N),(13,6,N))' '' '' '' '' '' '' '       履物　品名別　格外品合計表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG380 $USER_ID $JRCODE 5
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  得意先品名別　預り受払表  =====
#***  --- 預り受払ワーク 作成 ---
#***  INPUT  : TAZM,SNTRF
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HMG450 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,10,N),(32,7,N),(11,6,N))' '' '' '((17,5),(22,5),(27,5))' '' '' '' '            履物　預り受払表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 得意先品名別 預り受払表 ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG460 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  担当得意先別　請求残高明細表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0512' '(54,8,N)' '((54,8),(@          @),(186,1),(1,192),(1,192),(1,109))' '' '(186,1)' '' '((54,8,N,NE,@00000000@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@' '000000@))' '            請求残高　明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0256' '(1,20,C)' '((1,192),(@          @),(1,54))' '' '' '' '((13,1,N,NE,@5@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@000000@' '))' '            請求残高　明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 請求用売掛残高データ 抽出 ---
#***  INPUT  : DATEM,TM1,TSKF,WK0256NNN,WK0512NNN
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HKG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,4,N)' '' '' '((5,9),(14,8),(22,9),(31,9),(40,7))' '' '' '' '            請求残高　明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((47,2,N),(1,4,N))' '' '' '' '' '' '' '        担当別  請求残高　明細表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先別 請求残高明細表 ---
#***  INPUT  : DATEM,TM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG120 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  担当得意先別　販売実績表(消費税)  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '        担当得意先別　販売実績表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先別 販売実績表 ---
#***  INPUT  : DATEM,TM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG610 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  担当得意先別　請求明細表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SMF' 'WK0128' '((93,2,N),(1,4,N),(5,8,N),(83,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '               請求明細表               '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先別 請求明細表 ---
#***  INPUT  : DATEM,TM1,TTM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG060 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  担当日付別　入金明細表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURF' 'WK0128' '((39,2,N),(1,12,N),(21,2,N),(24,8,N),(32,6,N))' '((1,102),(@          @),(1,16))' '' '(13,8)' '' '' '' '   担当日付別入金明細表（集金予定用）   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当日付別 入金明細表 ---
#***  INPUT  : DATEM,TM1,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG310 $USER_ID $JRCODE 00 99 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  担当得意先別　売上粗利集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      担当得意先別　売上粗利集計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先別 売上粗利集計表 ---
#***  INPUT  : DATEM,TM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  部門担当得意先別　売上粗利集計表  =====
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '      担当得意先別　売上粗利集計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 部門担当得意先別 売上粗利集計表 ---
#***  INPUT  : DATEM,TM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  担当得意先分類別　売上粗利集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    担当得意先分類別　売上粗利集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG030 $USER_ID $JRCODE 7 00 99 1
source ../job/CRC_LIBRARY.sh
#***  =====  担当品種別　売上粗利集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '       担当品種別　売上粗利集計表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当品種別 売上粗利集計表 ---
#***  INPUT  : DATEM,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG060 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  担当得意先品種別　売上粗利集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    担当得意先品種別　売上粗利集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先品種別 売上粗利集計表 ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**      履物参考表    *****************************************************
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        分類　集計ファイル　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '' '' '' '' '' '' '           履物　品種別受払表           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG610 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
#**
../exec/HMG000 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      担当得意先別　売上粗利集計表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    担当得意先品種別　売上粗利集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 担当得意先品種別 売上粗利集計表 ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 6
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** ＊＊＊＊＊＊＊＊　 工      品 　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
#***  =====  工品表紙  ========
set ABORT=0;
../exec/HKG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  工品他日計表  =====
#***  INPUT  : DATEM,TTM,CALNM,URIRF,NYURF,BUMON-K,URIF,NYUF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHD610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品用途区分別 販売実績表  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品用途区分別 製品受払表  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
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
#***  =====  工品マット他 販売実績表  =====
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG040 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
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
#***  =====  工品他 得意先品名別 売上集計表  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品他 得意先品名別 売上集計表 ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 0
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
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
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
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 不良統計表 ---
#***  INPUT  : DATEM,KHM,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG350 $USER_ID $JRCODE '     ' 99999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  防振仕掛品日付別　受払表  ========
#***  --- 工品品名受払ワーク 作成 ---
#***  INPUT  : KHTM1,URIRF,KNHRF
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/KHG570 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,5,C),(6,6,N))' '' '' '((12,6),(18,6),(24,6),(30,6),(36,6))' '' '' '' '            工品　製品受払表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 防振仕掛品日付別 受払表 ---
#***  INPUT  : DATEM,KHM,KHTM1,KKYF,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG580 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- マット他仕掛品日付別 受払表 ---
#***  INPUT  : DATEM,KHM,KHTM1,KKYF,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG580 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品品名別  販売実績受払表(参考)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  工品他 得意先品名別 売上集計表（参考）  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- 工品他 得意先品名別 売上集計表（参考） ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***＜＜＜＜＜＜＜＜＜＜   ＥＸＣＥＬ　変換   ＞＞＞＞＞＞＞＞＞＞＞＞＞＞＞
#**     ＜履物分類別　販売実績表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        分類　集計ファイル　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=J3,J3,J8,S6,S9,S6,S9,S6,S9,S5V1,C177 PB1=../tmp/HGBUG.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=J,J,J,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ＜履物分類別　製品受払表＞
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=J3,J3,J8,S7,S9,S7,S9,S7,S9,S7,S9,S7,S9,C148 \
PB1=../tmp/HGBUH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=J,J,J,S,S,S,S,S,S,S,S,S,S,C \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ＜履物品名別　製品受払表４＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           履物　品種別受払表           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG620 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,N5,S5,S7,S10,S7,S10,S7,S10,S7,S10,S10,S3V1,C110 \
PB1=../tmp/HGHUH4.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ＜履物品名別　製品受払表６＞
set ABORT=0;
../exec/HMG620 $USER_ID $JRCODE 0 1 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,N5,S5,S7,S10,S7,S10,S7,S10,S7,S10,S10,S3V1,C110 \
PB1=../tmp/HGHUH6.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ＜担当得意先品名別　売上粗利集計表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    担当得意先品種別　売上粗利集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 9
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,C6,J24,S7,S9,S6,S9,S5,S9,S3V1,C95 \
PB1=../tmp/HGTTH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,C,J,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      担当得意先別　販売実績表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S3V1,S9,S9,S9,S3V1,S9,S9,S9,S3V1,S9,S9,S9,S3V1,C74 \
PB1=../tmp/HTTHG.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            履物　在庫明細表            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD620 $USER_ID $JRCODE 6 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,S7,S7,C120 \
PB1=../tmp/HZAIKO.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ＜履物品名サイズ別受払表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0256' '((248,2,N),(250,1,N),(244,3,N),(7,6,N))' '' '' '' '' '' '' '          品名サイズ別　受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG730 $USER_ID $JRCODE 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,J2,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,N5,S9,C116 \
PB1=../tmp/HHSUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ＜年間履物品名サイズ別受払表＞
set ABORT=0;
../exec/HMY910 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((1,7,N),(267,6,N))' '' '' '' '' '' '' '        年間品名サイズ別　受払表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMY920 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0768' 'WK0512' '((184,2,N),(186,1,N),(180,3,N),(1,6,N))' '(1,512)' '' '' '' '' '' '        年間品名サイズ別　受払表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY930 $USER_ID $JRCODE 1 000000 999999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,J2,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,N5,S10,C115 \
PB1=../tmp/NHHSUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ＜工品他得意先品名別　売上集計表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG450 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=C4,J26,C5,C20,S7V2,S6V2,S9,S9,S8,S3V1 \
PB1=../tmp/KTHUS.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,C,C,S,S,S,S,S,S PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**     ＜工品品名別　製品受払表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KHTM1' 'WK0256' '(1,8,C)' '((1,170),(@          @),(1,76))' '' '' '' '' '' '        　工品品名別　製品受払表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG025 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0512000 PA4=1 \
PA5=C2,C5,J20,N6V2,S7V2,S9,S7V2,S9,S7V2,S9,S7V2,S9,S9,S9,C367 \
PB1=../tmp/KGHUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**     ＜前年対比＞
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
../exec/HMY160 $USER_ID $JRCODE 1 0 1 1 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/KGTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
../exec/HMY160 $USER_ID $JRCODE 1 0 1 2 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HKTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**
#**     ＜前年対比一般ワーク＞
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 1 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAII.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ＜前年対比教育＞
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 2 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  担当得意先年間月別　売上・粗利対比表  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAIK.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ＜担当得意先売掛残高明細表＞
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '        担当得意先別　販売実績表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG610 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S10,S8,S10,S8,S9,S7,S10,S8,S10,S8,C110 \
PB1=../tmp/HKURIK.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
