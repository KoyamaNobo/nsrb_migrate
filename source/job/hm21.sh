#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMM210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 000) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=STENM-D_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_
#SAV=_NXT=_
#/> ;
ENDJOB:
