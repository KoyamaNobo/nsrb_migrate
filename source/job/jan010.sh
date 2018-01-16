#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ������������������������������������������������   '
echo '                 ��                                            ��   '
echo '                 ���@�@�@�@�@�i�`�m�R�[�h�@�ăZ�b�g�@�@�@�@�@�@��   '
echo '                 ��                                            ��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@�@CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������          '
f5orf9 ; if ($status == 1) exit 1;
#**
set ABORT=0;
NFCNV 'MN1=C MN2=DA PA1=MSD PA3=WK0064008 PA4=1 \
PA5=N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N1,N6,N1,N2,C10,C32 \
PB1=../tmp/jan.csv PB2=CSV1 PB3=PROTECT PB4=COMMA \
PB7=S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,C,C PB9=COPY MN1=Q '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/JAN01U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
