#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HKSRF' 'WK0064' '((23,1,N),(1,8,N),(27,6,N))' '((1,32),(@          @),(1,22))' '' '((9,6),(15,8))' '' '' '' '      教育協議会会費　請求用リスト      '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMK100 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'HKSRF' 'SHA' 'WK0064' 'CRE' '(24,1),(5,4),(9,6,S),(@00000@),(15,8,S),(@000000000@),(@0000000@),(27,6)' ',(@          @),(@        @)' '' '' '' '' '     教育振興会会費請求ワーク作成１     '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,5,N),(41,6,N))' '' '' '((6,6),(17,8))' '' '' '' '     教育振興会会費請求ワーク作成１     '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMK160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($JRCODE == 100) then
  goto B
endif
A:
set ABORT=0;
set NORMAL=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(32,2,S,D)' '' '' '' '' '' '' '     教育振興会会費請求ワーク作成２     '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($NORMAL == 1) then
  goto C
endif
B:
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(32,2,S)' '' '' '' '' '' '' '     教育振興会会費請求ワーク作成２     '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
C:
set ABORT=0;
../exec/HMK170 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,5,N)' '' '' '' '' '' '' '      教育振興会会費請求用　明細表      '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMK120 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '10' 'WK0064' 'SHA' 'HKKF' 'CRE' '(1,5),(@0@),(6,6,S),(@0@),(17,8,S),(34,7,S),(@00000000@),(41,6)' '' '' '' '' '' '     教育振興会会費請求ファイル作成     '
source ../job/CRC_LIBRARY.sh
ENDJOB:
