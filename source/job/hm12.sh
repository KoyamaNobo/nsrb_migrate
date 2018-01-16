#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HN010M $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 000) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=THTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
ENDJOB:
