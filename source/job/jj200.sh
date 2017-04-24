#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                                                                    '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊　　　　　受注関係　日次更新　　　　　　　　＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
if (`echo "$<" | grep F9 | wc -l ` == 1) exit
set ABORT=0;
../exec/JT690U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JT200U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTO23U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=JSJD_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
echo '                                                                   '
echo '                                                                   '
echo '            【　　出荷実績累積Ｆ　生成　　】                       '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=JSJDRF ODE=MSD OFI=JSJDRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((245,8,N),(1,15,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JT205U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#echo '                                                                   '
#echo '                                                                   '
#echo '              【　　トラスコ他統一伝票Ｆ　生成　　】               '
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=TDIFD_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
echo '                                                                   '
echo '                                                                   '
echo '            【　　ワークマンＥＤＩ累積Ｆ　生成　　】               '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=WMJCRF ODE=MSD OFI=WMJCRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((1302,6,N),(652,9,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            【　　ＪＣＡ手順（ワークマン）累積Ｆ　生成　　】       '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=JCAWRF ODE=MSD OFI=JCAWRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=(334,8,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            【　　ＪＣＡ手順（ナフコ）累積Ｆ　生成　　】           '
SORT 'SRT=           IDE=MSD IFI=JCANRF ODE=MSD OFI=JCANRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=(334,8,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            【　　ナフコ数量累積Ｆ　生成　　】                     '
SORT 'SRT=           IDE=MSD IFI=NSURYORF ODE=MSD OFI=NSURYORF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((46,8,N),(13,11,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '              【　　ナフコ箱数累積Ｆ　生成　　】                   '
SORT 'SRT=           IDE=MSD IFI=NHAKORF ODE=MSD OFI=NHAKORF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((37,8,N),(13,4,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '            【　　ナフコ数量訂正累積Ｆ　生成　　】                 '
SORT 'SRT=           IDE=MSD IFI=NTEISEIRF ODE=MSD OFI=NTEISEIRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((121,8,N),(13,11,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '         【　　ＪＣＡ手順（赤ちゃん本舗）累積Ｆ　生成　　】        '
SORT 'SRT=           IDE=MSD IFI=JCAARF ODE=MSD OFI=JCAARF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((129,6,N),(135,4,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#echo '                                                                   '
#echo '                                                                   '
#echo '              【　　主婦の友発注データ　生成　　】   　　          '
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=SK-HAT-D_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
set ABORT=0;
../exec/JTN38U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JTN51U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
