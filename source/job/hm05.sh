#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMM110 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($JRCODE == 000) then
  goto ENDJOB
endif
if($JRCODE == 100) then
  goto B
endif
if($ABORT == 1) then
  goto ENDJOB
endif
A:
set ABORT=0;
../exec/HMM015 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($JRCODE == 050) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#B/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=HIMD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=HHTFD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=THTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
ENDJOB:
