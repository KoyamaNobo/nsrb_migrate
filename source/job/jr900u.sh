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
../exec/PRN00U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
echo '                                                                    '
echo '            �@        ������������������������������������          '
echo '                      ��  �@�@�@�@�@�@�@�@�@�@�@�@�@�@�@���@        '
echo '                      ���@�@�@ �f�@�[�@�^�@�ہ@�� �@�@�@��          '
echo '                �@�@�@���@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@���@�@�@�@�@'
echo '                �@�@�@�������������������������������������@�@�@�@�@'
echo '                                                                    '
echo '                �@�@�@�J�[�g���b�W���C�e�[�v���Z�b�g���ĉ������@    '
echo '                                                                    '
echo '                      �����n�j�H                                    '
echo '                                                                    '
echo '                        �͂�    --->  �@�@CTRL + F5 ������          '
echo '                        ������  --->  �@�@CTRL + F9 ������          '
echo '                                                                    '
echo '                                          ���������ĉ�����          '
f5orf9 ; if ($status == 1) exit 1;
echo '                                                                    '
echo '            �@        ������������������������������������          '
echo '                      ��  �@�@�@�@�@�@�@�@�@�@�@�@�@�@�@���@        '
echo '                      ���@�@�@ �f�@�[�@�^�@�ہ@�� �@�@�@��          '
echo '                �@�@�@���@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@���@�@�@�@�@'
echo '                �@�@�@���@�@�@�@   ���@�s�@���@�@ �@�@�@���@�@�@�@�@'
echo '                �@�@�@���@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@���@�@�@�@�@'
echo '                �@�@�@�������������������������������������@�@�@�@�@'
echo '                                                                    '
set ABORT=0;
source ../job/db_bkup.sh "jr900u.sh" \
         'KAMOKU-KNG' 'KAMOKU-K'  'BS-K'     'PL-K'      'GENKA-F' \
         'BUMON-K'    'BANK-K'    'NFTORI-M' 'TEKIYO-K'  'KAZAN-K' \
         'HOZAN-K'    'BUZAN-K'   'KEIHI-K'  'GEYOKIN-M' 'BPLHAI-K' \
         'BGNHAI-K'   'BKHHAI-K'  'FCONTRL'  'SIWAKE-I'  'SIWAKE-D' \
         'SIWAKE-H1'  'BUPL-K'    'BUGEN-K'  'BPL-PRN'   'BGN-PRN' \
         'BKH-PRN'    'ZAN-K'     'NS-KES'   'NS-DNO'    'ZDF' 'ZDYR'
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PR700U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../job/mr700a.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../job/mr700c.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PR800U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PR805U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PR810U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/PR815U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/PR820U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
