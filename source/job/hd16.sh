#!/bin/tcsh
source ../job/RC_INIT.sh
#**
set ABORT=0;
../exec/HMD130 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=BB-STRAN IGN= IFO=PROTECT ODE=MSD OCI= OFI=STRAN OGN= LST=NO ERR=ABORT MOD=ADD UMD=NO GTR= DEL=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMD030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HN290U $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
echo '                 ������������������������������������������������   '
echo '                 ��                                            ��   '
echo '                 ���@�@�@�@�@�a�a�|�r�s�q�`�m�@�N���A�@�@�@�@�@��   '
echo '                 ��                                            ��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@  CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ :     CTRL + F9 �������@�@      '
f5orf9 ; if ($status == 1) exit 1;
BB:
FLCNV 'IDE=NO ODE=MSD OCI= OFI=BB-STRAN OGN= CMD=BOTH CLR=NO SAV= NXT= '
source ../job/CRC_LIBRARY.sh
ENDJOB:
