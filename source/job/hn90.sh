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
echo '                 ＊　　　　得意先品名別単価マスタ　セーブ　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=THTM1 IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-THTM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=DATA GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HN900M $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=THTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
ENDJOB:
