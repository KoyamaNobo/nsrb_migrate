#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HKM500 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 000) then
  goto ENDJOB
endif
if($JRCODE == 150) then
  goto A
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKE010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 100) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#A:
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=TD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=THTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
ENDJOB:
