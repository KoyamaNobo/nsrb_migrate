#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                 ����������������������������������������������   '
echo '                 ��                                          ��   '
echo '                 ���@�@�H�i�@�d�|�i�@�I���t�@�C���@�N���A�@�@��   '
echo '                 ��                                          ��   '
echo '                 ����������������������������������������������   '
echo '                                                                  '
echo '                                                                  '
echo '                                                                  '
echo '                       ���s����     : �@�@CTRL + F5 ������        '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������        '
f5orf9 ; if ($status == 1) exit 1;
FLCNV 'IDE=NO     ODE=MSD    OFI=KHTNF CMD=BOTH CLR=NO'
source ../job/CRC_LIBRARY.sh
ENDJOB:
