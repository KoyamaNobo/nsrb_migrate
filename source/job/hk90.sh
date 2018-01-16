#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ������������������������������������������������   '
echo '                 ���@�@�@�@����U����֌W�f�[�^�@�Z�[�u�@�@�@�@��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@�@CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������          '
f5orf9 ; if ($status == 1) exit 1;
B:
echo ' ��������������������������������������������������������       '
echo ' ������        �@����U����f�[�^�@�Z�[�u          ������       '
echo ' �������@�@�@�@�@�@ �@�@ ( HKKF )    �@�@�@�@�@�@�@������       '
echo ' ��������������������������������������������������������       '
BB:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=HKKF IFO=SHARE ODE=MSD000 OFI=ZZHKKF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto C
endif
#***
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk90.sh" "ZZHKKF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto BB
endif
if($ABORT == 1) then
  goto J
endif
#***
C:
echo ' ��������������������������������������������������������       '
echo ' ������        �@����U����f�[�^�@�Z�[�u          ������       '
echo ' �������@�@�@�@�@�@  �@�@ ( HKKYF )  �@�@�@�@�@�@�@������       '
echo ' ��������������������������������������������������������       '
CC:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=HKKYF IFO=SHARE ODE=MSD000 OFI=ZZHKKYF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto D
endif
#***
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk90.sh" "ZZHKKYF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto CC
endif
if($ABORT == 1) then
  goto J
endif
#***
D:
echo ' ��������������������������������������������������������       '
echo ' ������        �@����U����f�[�^�@�Z�[�u          ������       '
echo ' �������@�@�@�@�@�@  �@�@ ( HKSRF )  �@�@�@�@�@�@�@������       '
echo ' ��������������������������������������������������������       '
DD:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=HKSRF IFO=SHARE ODE=MSD000 OFI=ZZHKSRF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto E
endif
#***
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk90.sh" "ZZHKSRF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto DD
endif
if($ABORT == 1) then
  goto J
endif
#***
E:
echo ' ��������������������������������������������������������       '
echo ' ������        �@����U����f�[�^�@�Z�[�u          ������       '
echo ' �������@�@�@�@�@�@   �@ ( HKSRYF )  �@�@�@�@�@�@�@������       '
echo ' ��������������������������������������������������������       '
EE:
set NORMAL=0;
FLCNV 'IDE=MSD000 IFI=HKSRYF IFO=SHARE ODE=MSD000 OFI=ZZHKSRYF LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO CLR=NO '
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto S
endif
#***
set NORMAL=0;
set ABORT=0;
source ../job/db_alloc.sh "hk90.sh" "ZZHKSRYF"
source ../job/CRC_LIBRARY.sh
if($NORMAL == 1) then
  goto EE
endif
if($ABORT == 1) then
  goto J
endif
#***
J:
echo ' ��������������������������������������������������������       '
echo ' �������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������       '
echo ' ������         ���@�Ɩ��������ĉ������@��         ������       '
echo ' �������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������       '
echo ' ��������������������������������������������������������       '
if (`echo "$<" | wc -l ` == 1) exit
S:
set ABORT=0;
../exec/HMK900 $USER_ID $JRCODE 
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
echo '                                                                   '
echo '                                                                   '
echo '             �y�@�@����o�׏W�v�ݐσt�@�C���@�����@�@�z            '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=HKSRF ODE=MSD OFI=HKSRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=(27,6,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 200) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
echo '                                                                   '
echo '                                                                   '
echo '           �y�@�@�N�ԋ���o�׏W�v�ݐσt�@�C���@�����@�@�z          '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=HKSRYF ODE=MSD OFI=HKSRYF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=(27,6,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#***
echo '                                                                   '
echo '                                                                   '
echo '          �y�@�@�N�ԋ���U��������t�@�C���@�����@�@�z         '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=HKKYF ODE=MSD OFI=HKKYF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=(37,6,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 200) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
