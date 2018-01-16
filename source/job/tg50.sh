#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊　　　　手形月次　データセーブ・更新　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
f5orf9 ; if ($status == 1) exit 1;
B:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　 　　 ( BANKM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
BB:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=BANKM IFO=SHARE ODE=MSD000 OFI=ZZBANKM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto C
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZBANKM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto BB
endif
if($ABORT == 1) then
  goto K
endif
C:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　  　　 ( FKSM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
CC:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=FKSM IFO=SHARE ODE=MSD000 OFI=ZZFKSM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto D
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZFKSM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto CC
endif
if($ABORT == 1) then
  goto K
endif
D:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　  　　 ( RSRF )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
DD:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=RSRF IFO=SHARE ODE=MSD000 OFI=ZZRSRF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto E
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZRSRF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto DD
endif
if($ABORT == 1) then
  goto K
endif
E:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　   　 ( SHITM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
EE:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=SHITM IFO=SHARE ODE=MSD000 OFI=ZZSHITM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto F
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZSHITM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto EE
endif
if($ABORT == 1) then
  goto K
endif
F:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　    　 ( TDTM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
FF:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=TDTM IFO=SHARE ODE=MSD000 OFI=ZZTDTM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto G
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZTDTM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto FF
endif
if($ABORT == 1) then
  goto K
endif
G:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　    　 ( TNOM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
GG:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=TNOM IFO=SHARE ODE=MSD000 OFI=ZZTNOM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto H
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZTNOM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto GG
endif
if($ABORT == 1) then
  goto K
endif
H:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　    　 ( TYBF )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
HH:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=TYBF IFO=SHARE ODE=MSD000 OFI=ZZTYBF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto I
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZTYBF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto HH
endif
if($ABORT == 1) then
  goto K
endif
I:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　    　( UKETM )   　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
II:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=UKETM IFO=SHARE ODE=MSD000 OFI=ZZUKETM LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto J
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZUKETM"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto II
endif
if($ABORT == 1) then
  goto K
endif
J:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　手　形　　データ　セーブ          ＊＊＊       '
echo ' ＊＊＊　　　　　　  　( RNOUSTYR )  　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
JJ:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=RNOUSTYR IFO=SHARE ODE=MSD000 OFI=ZZRNOUSTYR LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto S
endif
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "tg50.sh" "ZZRNOUSTYR"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto JJ
endif
if($ABORT == 1) then
  goto K
endif
K:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊　　　　　　　　　　　　　　　　　　　　　　＊＊＊       '
echo ' ＊＊＊         ＜　業務放棄して下さい　＞         ＊＊＊       '
echo ' ＊＊＊　　　　　　　　　　　　　　　　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
if($ABORT == 1) then
  goto ENDJOB
endif
S:
set ABORT=0
set NORMAL=0
../exec/TSG610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($JRCODE == 100) then
  goto T
endif
if($NORMAL == 1) then
  goto U
endif
if($ABORT == 1) then
  goto ENDJOB
endif
T:
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
echo ' ＊＊＊        　銀行入出金データ　生成            ＊＊＊       '
echo ' ＊＊＊　　　　　　　( RNOUSTYR )    　　　　　　　＊＊＊       '
echo ' ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊       '
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=RNOUSTYR IFO=SHARE ODE=MSD OFI=WK0256 LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
FLCNV 'IDE=MSD IFI=WK0256 IFO=SHARE ODE=MSD000 OFI=RNOUSTYR LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
U:
set ABORT=0;
../exec/CSRT50 '10' 'UKETM' 'WK0256' '((18,4,N),(159,4,N),(34,4,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '       受取手形残高ファイル　作成       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG430 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 '10' 'SHITM' 'WK0128' '((15,4,N),(121,4,N),(31,4,N),(1,4,N))' '' '' '' '' '' '' '       支払手形残高ファイル　作成       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG440 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG990 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
