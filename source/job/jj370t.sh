#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTN36U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
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
#**                                                                         00260
set ABORT=0;
FLCNV 'IDE=MSD IFI=JMST1 IFO=SHARE ODE=MSD OFI=DJMST LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO DEL=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00320
set ABORT=0;
FLCNV 'IDE=MSD IFI=JNSR2 IFO=SHARE ODE=MSD OFI=DJNSR LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO DEL=NO CLR=NO OUT= RNG= SEL=(64,4,P,NE,@0000000@) SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00390
set ABORT=0;
../exec/JT370U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00450
#/RUN #MIXGN,DEV=MSD,SIZ=16;                                                00460
#ACT=ALIGN_KDE=MSD_KCI=_KFI=JMSTD_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_ 00470
#NXT=_                                                                      00480
#/> ;                                                                       00490
#/: JRCODE EQ 255  JUMP=ENDJOB;                                             00500
#/: ABORT    JUMP=ENDJOB;                                                   00510
#**                                                                         00520
#/RUN #MIXGN,DEV=MSD,SIZ=16;                                                00530
#ACT=ALIGN_KDE=MSD_KCI=_KFI=JNSR_WKD=TEMPORARY_WPB=YES_SKO=NO_LST=NO_SAV=_  00540
#NXT=_                                                                      00550
#/> ;                                                                       00560
#/: JRCODE EQ 000  JUMP=A;                                                  00570
#/: JRCODE EQ 255  JUMP=ENDJOB;                                             00580
#/: ABORT    JUMP=ENDJOB;                                                   00590
#**                                                                         00600
echo '                                                                   '
echo '                                                                   '
echo '              ÅyÅ@Å@ëqï ç›å…îNä‘ó›êœÇeÅ@ê∂ê¨Å@Å@Åz                 '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=NJZAIYR ODE=MSD OFI=NJZAIYR WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((335,6,N),(1,8,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00710
A:
set ABORT=0;
../exec/JT375U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00770
set ABORT=0;
../exec/JT005U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
