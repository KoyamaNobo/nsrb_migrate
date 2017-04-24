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
NFCNV 'MN1=C MN2=DA PA1=MSD PA2= PA3=NAFCOD PA4=1 \
PA5=N2,N3,C6,C7,N2,C25,C25,N5,N5 PB1=../tmp/nafcod.csv PB2=CSV2 \
PB3=PROTECT PB4=COMMA PB7=S,S,C,C,S,C,C,S,S PB9=COPY MN1=Q '
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS13U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS42U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((64,1,N,D),(1,5,N),(63,1,N),(13,7,N))' '' '' '((20,4),(24,4),(28,4),(32,4),(36,4),(40,4),(44,4),(48,4),(52' ',4),(56,4))' '' '' '       ナフコ　出荷指図　自動変換       '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS47U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '11' 'NHAKOF' '(1,16,C)' '' '' '(35,2)' '' '' '' '       ナフコ　出荷指図　自動変換       '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA2= PA3=NHAKOF PA4=1 PA5=N6,N6,N3,C1,N6,N8,N4,N2 \
PB1=../tmp/HAKO.DAT PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=S,S,S,C,S,S,S,S \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA2= PA3=NSURYOF PA4=1 PA5=N6,N6,N3,N8,N6,N8,N4,N2,N2 \
PB1=../tmp/SYUKKA.DAT PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=S,S,S,S,S,S,S,S,S PB8=CREATE MN1=Q '
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/JTN35L $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
ENDJOB:
