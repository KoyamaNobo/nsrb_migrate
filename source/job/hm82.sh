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
echo '                 ＊　　　　　　月中前月末　評価替え　　　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　  CTRL + F5 を押下          '
echo '                     　実行しない　 :     CTRL + F9 を押下　　      '
if (`echo "$<" | grep F9 | wc -l ` == 1) exit;
set ABORT=0;
FLCNV 'IDE=MSD000 IFI=HIM1 IFO=SHARE ODE=MSD001 OFI=T-HIM1 LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
if($ABORT == 1) then
  goto ENDJOB
endif
#***
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=T-HIMD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: ABORT JUMP=ENDJOB;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#***
set ABORT=0;
../exec/HMM740 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMM760 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMM765 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMG965 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
