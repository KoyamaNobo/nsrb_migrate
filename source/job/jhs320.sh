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
../exec/JHS01U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=JCANF PA4=1 PA5=C256 \
PB1=../tmp/hachu.dat PB2=TXTJ PB3=PROTECT PB7=C PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TDNNRF' 'WK0064' '((257,8,N,D),(1,20,C))' '((257,8),(1,20),(@          @),(@          @),(@          @)' ',(@      @))' '' '' '' '' '       ナフコ受信データ　チェック       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS05U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS12U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '11' 'NAFCOD' '((21,25,C),(46,25,C),(3,3,C))' '' '' '' '' '' '' '     　  ナフコ訂正データ　作成　       '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA2= PA3=NAFCOD PA4=1 \
PA5=N2,N3,C6,N7,N2,C25,C25,N5,N5 \
PB1=../tmp/NAFCOD.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=S,S,C,S,S,C,C,S,S \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS31U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
