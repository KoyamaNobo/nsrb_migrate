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
echo '                 ＊　　　　履物・工品他　データセーブ　　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                     カートリッジ磁気テープをセットして下さい       '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
f5orf9 ; if ($status == 1) exit 1;
source ../job/db_bkup.sh "hk71.sh" \
              "CALNF"    "HUHM"     "HHTF1"     "HKBM"       "SNTRF" \
              "UTRF"     "SKDF"     "SKDKF"     "TSKF"       "TAZM" \
              "SSRYF"    "SUSRYF"   "HPYRF"     "HIYF"       "HIMYR" \
              "HKSRF"    "HKSRYF"   "HKKYF"     "STRANYR"    "R-STRANYRK" \
              "STRAN-3"  "UTRYR"    "HHTYR"     "HTIM"       "KHM" \
              "KHTM1"    "KKBM"     "URIRF" \
              "KNHRF"    "URIRYR"   "KNHRYR" \
              "KHMYR"    "KHTMYR" \
              "KHTNF"    "SMF"      "NYURYR"    "TUKF1" \
              "TUKOF"    "TTMYR"    "NYURF"     "TTM"        "TZNTM" \
              "TZNTMYR"  "TAZMYR"   "TMYR"      "SIWAKE-IW"  "THTM1"    "HFTSF"
source ../job/CRC_LIBRARY.sh
B:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　履　物　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　　　　 ( T M )  　　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
BB:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=TM1 IFO=SHARE ODE=MSD000 OFI=ZZTM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto C
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk71.sh" "ZZTM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto BB
endif
if($ABORT == 1) then
  goto J
endif
C:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　履　物　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　　　　( H I M ) 　　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
CC:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=HIM1 IFO=SHARE ODE=MSD000 OFI=ZZHIM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto D
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk71.sh" "ZZHIM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto CC
endif
if($ABORT == 1) then
  goto J
endif
D:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　履　物　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　　　　( T C M ) 　　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
DD:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=TCM IFO=SHARE ODE=MSD000 OFI=ZZTCM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto X
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk71.sh" "ZZTCM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto DD
endif
if($ABORT == 1) then
  goto J
endif
J:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊　　　　　　　　　　　　　　　　　　　　　　＊＊＊       '
echo ' ＊＊＊         ＜　業務放棄して下さい　＞         ＊＊＊       '
echo ' ＊＊＊　　　　　　　　　　　　　　　　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
if (`echo "$<" | wc -l ` == 1) exit;
#***
X:
set ABORT=0;
../exec/HMG900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG910 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 8
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 9
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'UTRF' 'SHA' 'WK0064' 'CRE' '(16,6),(@00@),(63,5,S),(@0@),(68,8,S),(@0@),(76,8,S),(85,1),(88,9),(84,1' '),(@000@),(8,6),(@          @),(1,3) '' '' '' '' '           ＳＵＳＲＹＦ　変換           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,6,N),(32,1,N),(42,1,N))' '' '' '((7,7),(14,9),(23,9))' '' '' '' '           ＳＵＳＲＹＦ　変換           '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 7
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG990 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,10,N)' '' '' '((11,7),(18,10),(28,10))' '' '' '' '            ＳＳＲＹＦ　変換            '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG920 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'SNTRF' 'SHA' 'WK0064' 'CRE' '(16,10),(@00@),(57,5,S),(@00@),(67,8,S),(92,2),(85,6),(76,1),(8,6),(@   ' '       @),(1,12)' '' '' '(7,1,N,NE,@9@)A(76,1,N,GE,@1@)A(76,1,N,LE,@2@)' '' '            ＨＰＹＲＦ　変換            '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((36,1,N),(1,10,N))' '' '' '((11,7),(18,10))' '' '' '' '            ＨＰＹＲＦ　変換            '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
../exec/HMG920 $USER_ID $JRCODE 4
source ../job/CRC_LIBRARY.sh
#***
set ABORT=0;
../exec/HMG950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/KHG910 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG940 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((24,1,N),(1,8,N))' '' '' '((9,6),(15,8))' '' '' '' '       教育出荷累積ファイル　変換       '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '10' 'WK0064' 'SHA' 'HKSRF' 'ADD' '(1,32)' '' '' '' '' '' '       教育出荷累積ファイル　変換       '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMT700 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        分類　集計ファイル　作成        '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMT710 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMG960 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/KHG930 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HKG900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HKG910 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=TUKFD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
#***
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=SKDFD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
#***
set ABORT=0;
../exec/HKG980 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HKG950 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG930 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=STRAN-3_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_
#SAV=_NXT=_
#/> ;
ENDJOB:
