#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
#***�����������������@ �������E�H�i���@�A���p��  �@������������������������
#*** �����������������@ ��  ��  �� �@��������������������������������������
#***  =====  �����\��  ========
#***  INPUT  : DATEM
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG000 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ����ʓ��v�W�v�\  =====
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,URIRF
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG400 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
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
#***  =====  ���匎�ʔ���Δ�\  ==========
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TZNTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=TZNTPM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
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
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
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
#***  =====  �H�i�i����  �̔����ѕ\(����)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�}�b�g�� �̔����ѕ\  =====
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG040 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �H�i�a����@�󕥕\  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�� ���Ӑ�i���� ����W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    �H�i���@���Ӑ�i���ʁ@����W�v�\    '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- �H�i�� ���Ӑ�i���� ����W�v�\ ---
#***  INPUT  : DATEM,TM1,KHM,JM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�i����  �̔����ю󕥕\(�Q�l)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�� ���Ӑ�i���� ����W�v�\�i�Q�l�j  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    �H�i���@���Ӑ�i���ʁ@����W�v�\    '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- �H�i�� ���Ӑ�i���� ����W�v�\�i�Q�l�j ---
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
#***  =====  ����  �i��E�i���ʁ@���i�󕥕\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           �����@�i��ʎ󕥕\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޕi��ʐ��i�󕥕\ ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 0 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޕi���ʐ��i�󕥕\ ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 1 0 0 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ���ޕi���ʁ@�݌ɖ��ו\  =====
#***  --- �݌ɕ\���[�N �쐬 ---
#***  INPUT  : HHTF1,HIM1
#***  OUTPUT : WK0128NNN
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            �����@�݌ɖ��ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޕi���ʍ݌ɖ��ו\ ---
#***  INPUT  : DATEM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
../exec/HMD620 $USER_ID $JRCODE 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
#***  =====  ���Y�֌W�@��\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UTRF' 'WK0128' '((85,1,N),(16,4,N),(84,1,N))' '' '' '' '' '' '' '            �����@���Y���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���Y�敪�i���� ���Y���ו\ ---
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �����i���ʕs�Ǖԕi���v�\�@��\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0128' '(1,7,N)' '' '' '' '' '((7,1,N,LE,@6@)A(128,1,N,EQ,@0@))' '' '      �����@�i���ʁ@�s�Ǖԕi���v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((89,2,N),(125,1,N),(85,2,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '((57,5),(67,8))' '' '((20,4,N,NE,@9999@)A(76,1,N,EQ,@2@))' '' '      �����@�i���ʁ@�s�Ǖԕi���v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG375 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((48,2,N),(58,1,N),(44,2,N),(60,1,N),(13,6,N))' '' '' '' '' '' '' '      �����@�i���ʁ@�s�Ǖԕi���v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG380 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �����i���ʊi�O�i���v�\�@��\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UTRF' 'WK0064' '(16,6,N)' '((8,8),(@0000@),(16,6),(63,5),(@00000@),(@00000000@),(@00000' '00@),(88,6),(@0000@),(@0000000@),(@    @))' '(63,5)' '' '((16,4,N,LE,@9999@)A(84,1,N,EQ,@5@))' '' '       �����@�i���ʁ@�i�O�i���v�\       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG375 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((48,2,N),(58,1,N),(44,2,N),(60,1,N),(13,6,N))' '' '' '' '' '' '' '       �����@�i���ʁ@�i�O�i���v�\       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG380 $USER_ID $JRCODE 5
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ���Ӑ�i���ʁ@�a��󕥕\  =====
#***  --- �a��󕥃��[�N �쐬 ---
#***  INPUT  : TAZM,SNTRF
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/HMG450 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,10,N),(32,7,N),(11,6,N))' '' '' '((17,5),(22,5),(27,5))' '' '' '' '            �����@�a��󕥕\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���Ӑ�i���� �a��󕥕\ ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG460 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ�ʁ@�����c�����ו\  =====
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
#***  --- �����p���|�c���f�[�^ ���o ---
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((47,2,N),(1,4,N))' '' '' '' '' '' '' '        �S����  �����c���@���ו\        '
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
../exec/HKG120 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ�ʁ@�̔����ѕ\(�����)  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '        �S�����Ӑ�ʁ@�̔����ѕ\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� �̔����ѕ\ ---
#***  INPUT  : DATEM,TM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG610 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ�ʁ@�������ו\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SMF' 'WK0128' '((93,2,N),(1,4,N),(5,8,N),(83,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '               �������ו\               '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� �������ו\ ---
#***  INPUT  : DATEM,TM1,TTM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG060 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �S�����t�ʁ@�������ו\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURF' 'WK0128' '((39,2,N),(1,12,N),(21,2,N),(24,8,N),(32,6,N))' '((1,102),(@          @),(1,16))' '' '(13,8)' '' '' '' '   �S�����t�ʓ������ו\�i�W���\��p�j   '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����t�� �������ו\ ---
#***  INPUT  : DATEM,TM1,HKBM,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG310 $USER_ID $JRCODE 00 99 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ�ʁ@����e���W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      �S�����Ӑ�ʁ@����e���W�v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� ����e���W�v�\ ---
#***  INPUT  : DATEM,TM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ����S�����Ӑ�ʁ@����e���W�v�\  =====
#***
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '      �S�����Ӑ�ʁ@����e���W�v�\      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ����S�����Ӑ�� ����e���W�v�\ ---
#***  INPUT  : DATEM,TM1,WK0128NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HKG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ敪�ޕʁ@����e���W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ敪�ޕʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG030 $USER_ID $JRCODE 7 00 99 1
source ../job/CRC_LIBRARY.sh
#***  =====  �S���i��ʁ@����e���W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '       �S���i��ʁ@����e���W�v�\       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S���i��� ����e���W�v�\ ---
#***  INPUT  : DATEM,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG060 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �S�����Ӑ�i��ʁ@����e���W�v�\  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i��� ����e���W�v�\ ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**      �����Q�l�\    *****************************************************
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '' '' '' '' '' '' '           �����@�i��ʎ󕥕\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG610 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
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
#**
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i��� ����e���W�v�\ ---
#***  INPUT  : DATEM,TM1,HIM1,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 6
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** �����������������@ �H      �i �@��������������������������������������
#***  =====  �H�i�\��  ========
set ABORT=0;
../exec/HKG000 $USER_ID $JRCODE 1
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
#***  =====  �H�i�i����  �̔����ѕ\(����)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�}�b�g�� �̔����ѕ\  =====
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG040 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  �H�i�a����@�󕥕\  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
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
../exec/KHG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �����p�p���ו\  ========
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((47,2,N),(11,5,C))' '' '' '' '' '(1,2,N,LE,@28@)' '' '           �����E�p�p�@���ו\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �����p�p���ו\ ---
#***  INPUT  : DATEM,KHM,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �s�Ǔ��v�\  ========
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRF' 'WK0064' '((11,5,C),(1,2,N))' '' '' '' '' '((1,2,N,GE,@01@)A(1,2,N,LE,@28@))' '' '        �@     �s�ǖ��ו\               '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �s�Ǔ��v�\ ---
#***  INPUT  : DATEM,KHM,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG350 $USER_ID $JRCODE '     ' 99999 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �h�U�d�|�i���t�ʁ@�󕥕\  ========
#***  --- �H�i�i���󕥃��[�N �쐬 ---
#***  INPUT  : KHTM1,URIRF,KNHRF
#***  OUTPUT : WK0064NNN
set ABORT=0;
../exec/KHG570 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,5,C),(6,6,N))' '' '' '((12,6),(18,6),(24,6),(30,6),(36,6))' '' '' '' '            �H�i�@���i�󕥕\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �h�U�d�|�i���t�� �󕥕\ ---
#***  INPUT  : DATEM,KHM,KHTM1,KKYF,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG580 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  --- �}�b�g���d�|�i���t�� �󕥕\ ---
#***  INPUT  : DATEM,KHM,KHTM1,KKYF,WK0064NNN
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG580 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�i����  �̔����ю󕥕\(�Q�l)  ========
#***  INPUT  : DATEM,KHM,KHTM2
#***  OUTPUT : PRN999
set ABORT=0;
../exec/KHG030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#***  =====  �H�i�� ���Ӑ�i���� ����W�v�\�i�Q�l�j  =====
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    �H�i���@���Ӑ�i���ʁ@����W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �H�i�� ���Ӑ�i���� ����W�v�\�i�Q�l�j ---
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
#***��������������������   �d�w�b�d�k�@�ϊ�   ������������������������������
#**     ���������ޕʁ@�̔����ѕ\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=J3,J3,J8,S6,S9,S6,S9,S6,S9,S5V1,C177 PB1=../tmp/HGBUG.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=J,J,J,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ���������ޕʁ@���i�󕥕\��
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=J3,J3,J8,S7,S9,S7,S9,S7,S9,S7,S9,S7,S9,C148 \
PB1=../tmp/HGBUH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=J,J,J,S,S,S,S,S,S,S,S,S,S,C \
PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     �������i���ʁ@���i�󕥕\�S��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           �����@�i��ʎ󕥕\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG620 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,N5,S5,S7,S10,S7,S10,S7,S10,S7,S10,S10,S3V1,C110 \
PB1=../tmp/HGHUH4.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     �������i���ʁ@���i�󕥕\�U��
set ABORT=0;
../exec/HMG620 $USER_ID $JRCODE 0 1 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,N5,S5,S7,S10,S7,S10,S7,S10,S7,S10,S10,S3V1,C110 \
PB1=../tmp/HGHUH6.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ���S�����Ӑ�i���ʁ@����e���W�v�\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 9
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,C6,J24,S7,S9,S6,S9,S5,S9,S3V1,C95 \
PB1=../tmp/HGTTH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,C,J,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      �S�����Ӑ�ʁ@�̔����ѕ\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S3V1,S9,S9,S9,S3V1,S9,S9,S9,S3V1,S9,S9,S9,S3V1,C74 \
PB1=../tmp/HTTHG.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            �����@�݌ɖ��ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD620 $USER_ID $JRCODE 6 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,S7,S7,C120 \
PB1=../tmp/HZAIKO.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     �������i���T�C�Y�ʎ󕥕\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HHTF1' 'WK0256' '((248,2,N),(250,1,N),(244,3,N),(7,6,N))' '' '' '' '' '' '' '          �i���T�C�Y�ʁ@�󕥕\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG730 $USER_ID $JRCODE 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,J2,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,N5,S9,C116 \
PB1=../tmp/HHSUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ���N�ԗ����i���T�C�Y�ʎ󕥕\��
set ABORT=0;
../exec/HMY910 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((1,7,N),(267,6,N))' '' '' '' '' '' '' '        �N�ԕi���T�C�Y�ʁ@�󕥕\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HMY920 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0768' 'WK0512' '((184,2,N),(186,1,N),(180,3,N),(1,6,N))' '(1,512)' '' '' '' '' '' '        �N�ԕi���T�C�Y�ʁ@�󕥕\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY930 $USER_ID $JRCODE 1 000000 999999 1
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C6,J24,J2,C1,S6,S6,S6,S6,S6,S6,S6,S6,S6,S6,S7,N5,S10,C115 \
PB1=../tmp/NHHSUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,J,C,S,S,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**     ���H�i�����Ӑ�i���ʁ@����W�v�\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    �H�i���@���Ӑ�i���ʁ@����W�v�\    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG450 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0128000 PA4=1 \
PA5=C4,J26,C5,C20,S7V2,S6V2,S9,S9,S8,S3V1 \
PB1=../tmp/KTHUS.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,J,C,C,S,S,S,S,S,S PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**     ���H�i�i���ʁ@���i�󕥕\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KHTM1' 'WK0256' '(1,8,C)' '((1,170),(@          @),(1,76))' '' '' '' '' '' '        �@�H�i�i���ʁ@���i�󕥕\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG025 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
set ABORT=0;
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0512000 PA4=1 \
PA5=C2,C5,J20,N6V2,S7V2,S9,S7V2,S9,S7V2,S9,S7V2,S9,S9,S9,C367 \
PB1=../tmp/KGHUH.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**     ���O�N�Δ䁄
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
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
../exec/HMY160 $USER_ID $JRCODE 1 0 1 1 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/KGTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
../exec/HMY160 $USER_ID $JRCODE 1 0 1 2 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HKTAIH.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**
#**     ���O�N�Δ��ʃ��[�N��
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 1 1
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
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAII.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ���O�N�Δ䋳�灄
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1 2 1
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
../exec/HMY160 $USER_ID $JRCODE 1 0 1 0 1 9 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S9,S9,S9,S9,S9,S9,S10,S10,C124 \
PB1=../tmp/HGTAIK.CSV \
PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA PB7=C,C,J,S,S,S,S,S,S,S,S,C PB8=CREATE \
MN1=Q '
source ../job/CRC_LIBRARY.sh
#**     ���S�����Ӑ攄�|�c�����ו\��
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '        �S�����Ӑ�ʁ@�̔����ѕ\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG610 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
NFCNV 'MN1=C MN2=AD PA1=MSD PA3=WK0256000 PA4=1 \
PA5=C2,C4,J26,S10,S8,S10,S8,S9,S7,S10,S8,S10,S8,C110 \
PB1=../tmp/HKURIK.CSV PB2=CSV2 PB3=EXCLUSIVE PB4=COMMA \
PB7=C,C,J,S,S,S,S,S,S,S,S,S,S,C PB8=CREATE MN1=Q '
source ../job/CRC_LIBRARY.sh
ENDJOB:
