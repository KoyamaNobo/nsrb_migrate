#!/bin/tcsh
source ../job/RC_INIT.sh
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
