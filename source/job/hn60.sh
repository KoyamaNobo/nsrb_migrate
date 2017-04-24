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
FLCNV 'IDE=MSD ICI= IFI=HHTF1 IGN= IFO=SHARE ODE=MSD OCI= OFI=TNF-HHTF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HUHM IGN= IFO=SHARE ODE=MSD OCI= OFI=TNF-HUHM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMN510 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((28,2,N),(32,1,N),(24,3,N),(1,6,N),(19,5,N))' '' '' '' '' '' '' '            履物　棚卸誤差表            '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN620 $USER_ID $JRCODE 11
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=J3,J3,J8,S7,S9,S7,S9,S7,S9,C52 PB1=../tmp/HTNBSA.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=J,J,J,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN610 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=C6,J24,S7,S9,S7,S9,S7,S9,C26 PB1=../tmp/HTNHSA.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,J,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0512' '((1,6,N),(13,1,N))' '((1,256),(@          @),(1,246))' '' '' '' '' '' '   品名サイズ別棚卸誤差ファイル　作成   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN410 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN420 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((80,2,N),(84,1,N),(76,3,N),(1,7,N),(85,1,N))' '' '' '' '' '' '' '        品名サイズ別  棚卸誤差表        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN430 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=C6,J24,J2,N1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,C2 \
PB1=../tmp/HTNSSA.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,J,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMN710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HHTF1 IGN= IFO=SHARE ODE=MSD OCI= OFI=TNR-HHTF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HUHM IGN= IFO=SHARE ODE=MSD OCI= OFI=TNR-HUHM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HIM1 IGN= IFO=SHARE ODE=MSD OCI= OFI=TNR-HIM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
