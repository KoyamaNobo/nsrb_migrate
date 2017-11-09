#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                 ������������������������������������������������   '
echo '                 ��                                            ��   '
echo '                 ���@�@�@�@�@�w���@�f�[�^�@�Z�[�u�@�@�@�@�@�@�@��   '
echo '                 ��                                            ��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                     �J�[�g���b�W���C�e�[�v���Z�b�g���ĉ�����       '
echo '                                                                    '
echo '                       ���s����     : �@�@CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������          '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
source ../job/db_bkup.sh "bgsv.sh" \
         'KBNOM' 'SM'     'SMD'   'STM'   'JM' \
         'JMD'   'JTM'    'JTMD'  'JKM'   'JSSRF' \
         'HARF'  'JSSRYR' 'HARYR' 'STYF'  'JTYF' \
         'HSSF'  'HSHFD'  'HSHF1' 'HSHF2' 'HSHF3' \
         'HSHNF'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
