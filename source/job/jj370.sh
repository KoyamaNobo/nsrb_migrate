#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
../exec/JTN36U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=JCON IGN= IFO=SHARE ODE=MSD OCI= OFI=BU-JCON OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=NJZAI IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-NJZAI OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=JNYZ IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-JNYZ OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=JMST1 IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-JMSTD OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=JNSR1 IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-JNSR OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NJZAI' 'WK0512' '((2,6,N),(8,1,N))' '((1,341),(@          @),(1,161))' '' '' '' '' '' '    ëqï ç›å…É}ÉXÉ^Å[Å@çáåvÇbÇgÇdÇbÇj    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN37U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/JT370U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=JMSTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT    JUMP=ENDJOB;
#**
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KCI=_KFI=JNSR_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 000  JUMP=A;
#/: JRCODE EQ 255  JUMP=ENDJOB;
#/: ABORT    JUMP=ENDJOB;
#**
echo '                                                                   '
echo '                                                                   '
echo '              ÅyÅ@Å@óöï®ëqå…ä‘à⁄ìÆÇeÅ@ê∂ê¨Å@Å@Åz                   '
A:
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=HSKIF ODE=MSD OFI=HSKIF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((8,8,N),(1,7,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
echo '                                                                   '
echo '                                                                   '
echo '              ÅyÅ@Å@éwê}ì¸óÕì˙ïtÇeÅ@ê∂ê¨Å@Å@Åz                     '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=J-DATE ODE=MSD OFI=J-DATE WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((1,6,N),(7,6,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/JT375U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
../exec/JT005U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
FLCNV 'IDE=NO ODE=MSD OCI= OFI=T-CHKF OGN= CMD=BOTH CLR=NO SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
