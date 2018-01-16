#!/bin/tcsh
source ../job/RC_INIT.sh
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=CRE_KDE=MSD_KCI=_KFI=R-STRANYR_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_
#SAV=_NXT=_
#/> ;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
set ABORT=0;
../exec/HKG810 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
