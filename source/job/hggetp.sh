#!/bin/tcsh
#***�����������������@ �e�S���ҁ@�P�[�i�`�S�j �@����������������������������
source ../job/RC_INIT.sh
#*** �����������������@ �Ё@�@�@�� �@��������������������������������������
#***  =====  ���ޔ̔����ѕ\�E���i�󕥕\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޔ̔����ѕ\ ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ސ��i�󕥕\ ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �H�i�p�r�敪�� �̔����ѕ\  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�p�r�敪�� ���i�󕥕\  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  ���匎�ʔ���Δ�\  ==========
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TZNTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=TZNTPM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���Ӑ�N�Ԕ̔����[�N �X�V ---
#***  INPUT  : DATEM,TTM,SNTRF
#***  I-O    : TZNTPM
set ABORT=0;
../exec/HMG990 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- ���Ӑ�N�Ԕ̔����[�N �W�v ---
#***  INPUT  : TZNTPM
#***  OUTPUT : WK0512NNN
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 0 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���匎�� ����Δ�\ ---
#***  INPUT  : DATEM,TM1,WK0512NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 0 0 1 2 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**           �u�Q�l�����v
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**
../exec/HMG000 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      �S�����Ӑ�ʁ@����e���W�v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
#***  =====  �H�i�� ���Ӑ�i���� ����W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    �H�i���@���Ӑ�i���ʁ@����W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �H�i�� ���Ӑ�i���� ����W�v�\ ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
#*** �����������������@ �@�@�V�@�@ �@��������������������������������������
#***  =====  ����ʓ��v�W�v�\  =====
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,URIRF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG400 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �������v�\  =====
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,UTRF,STRAN,NYUF,UTRAN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMD710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �H�i�����v�\  =====
#***  INPUT  : DATEM,TTM,CALNM,URIRF,NYURF,BUMON-K,URIF,NYUF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHD610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  ���ޔ̔����ѕ\�E���i�󕥕\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޔ̔����ѕ\ ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ސ��i�󕥕\ ---
#***  INPUT  : DATEM,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �H�i�p�r�敪�� �̔����ѕ\  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�p�r�敪�� ���i�󕥕\  =====
#***  INPUT  : DATEM,KKBM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  ���哾�Ӑ�ʁ@�����c�����ו\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0512' '(54,8,N)' '((54,8),(@          @),(186,1),(1,192),(1,192),(1,109))' '' '(186,1)' '' '((54,8,N,NE,@00000000@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@' '000000@))' '            �����c���@���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0256' '(1,20,C)' '((1,192),(@          @),(1,54))' '' '' '' '((13,1,N,NE,@5@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@000000@' '))' '            �����c���@���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �������|�c���f�[�^ ���o ---
#***  INPUT  : DATEM,TM1,TSKF,WK0256NNN,WK0512NNN
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HKG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '(1,4,N)' '' '' '((5,9),(14,8),(22,9),(31,9),(40,7))' '' '' '' '            �����c���@���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((49,1,N),(1,4,N))' '' '' '' '' '' '' '   �����  �����c���@���ו\  (�o���p)   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� �����c�����ו\ ---
#***  INPUT  : DATEM,TM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG120 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
