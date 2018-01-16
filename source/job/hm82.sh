#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ������������������������������������������������   '
echo '                 ��                                            ��   '
echo '                 ���@�@�@�@�@�@�����O�����@�]���ւ��@�@�@�@�@�@��   '
echo '                 ��                                            ��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@  CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ :     CTRL + F9 �������@�@      '
f5orf9 ; if ($status == 1) exit 1;
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
