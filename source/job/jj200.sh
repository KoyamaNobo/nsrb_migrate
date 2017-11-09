#!/bin/tcsh
if (${#argv} >= 1) then
    set USER_ID = ${argv[1]};
else
    set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                                                                    '
echo '                 ������������������������������������������������   '
echo '                 ���@�@�@�@�@�󒍊֌W�@�����X�V�@�@�@�@�@�@�@�@��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@�@CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������          '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
../exec/JT690U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JT200U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTO23U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=JSJD_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@�o�׎��їݐςe�@�����@�@�z                       '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=JSJDRF ODE=MSD OFI=JSJDRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=NO KEY=((245,8,N),(1,15,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JT205U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#echo '                                                                   '
#echo '                                                                   '
#echo '              �y�@�@�g���X�R������`�[�e�@�����@�@�z               '
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=TDIFD_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@���[�N�}���d�c�h�ݐςe�@�����@�@�z               '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=WMJCRF ODE=MSD OFI=WMJCRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((1302,6,N),(652,9,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@�i�b�`�菇�i���[�N�}���j�ݐςe�@�����@�@�z       '
set ABORT=0;
SORT 'SRT=           IDE=MSD IFI=JCAWRF ODE=MSD OFI=JCAWRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=(334,8,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@�i�b�`�菇�i�i�t�R�j�ݐςe�@�����@�@�z           '
SORT 'SRT=           IDE=MSD IFI=JCANRF ODE=MSD OFI=JCANRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=(334,8,N) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@�i�t�R���ʗݐςe�@�����@�@�z                     '
SORT 'SRT=           IDE=MSD IFI=NSURYORF ODE=MSD OFI=NSURYORF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((46,8,N),(13,11,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '              �y�@�@�i�t�R�����ݐςe�@�����@�@�z                   '
SORT 'SRT=           IDE=MSD IFI=NHAKORF ODE=MSD OFI=NHAKORF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((37,8,N),(13,4,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '                                                                   '
echo '            �y�@�@�i�t�R���ʒ����ݐςe�@�����@�@�z                 '
SORT 'SRT=           IDE=MSD IFI=NTEISEIRF ODE=MSD OFI=NTEISEIRF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((121,8,N),(13,11,N)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
echo '                                                                   '
echo '                                                                   '
echo '         �y�@�@�i�b�`�菇�i�Ԃ����{�܁j�ݐςe�@�����@�@�z        '
SORT 'SRT=           IDE=MSD IFI=JCAARF ODE=MSD OFI=JCAARF WKD=TEMPORARY WSZ= LST=NO SAV= NXT= NOV= DKY=FIFO KEY=((129,6,N),(135,4,C)) OUT= SUM= TOT= SEL= ALT= '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#echo '                                                                   '
#echo '                                                                   '
#echo '              �y�@�@��w�̗F�����f�[�^�@�����@�@�z   �@�@          '
#/RUN #MIXGN,DEV=MSD,SIZ=16;
#ACT=ALIGN_KDE=MSD_KFI=SK-HAT-D_WKD=TEMPORARY_WPB=SKI_SKO=NO_LST=NO_SAV=_
#NXT=_
#/> ;
#/: JRCODE EQ 255 JUMP=ENDJOB;
#/: ABORT JUMP=ENDJOB;
set ABORT=0;
../exec/JTN38U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JTN51U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/CSRT50 $USER_ID $JRCODE '55' 'JSTRRF' 'OSMF' '((142,6,N),(17,8,N),(25,7,N),(32,1,N),(138,1,N))' '((142,6),(17,8),(25,7),(32,1),(138,1),(224,3),(244,1),(@ @))' '' '(244,1)' '' '((17,6,N,GE,@201605@)A(142,6,N,NE,@000000@))' '' '      ����󑗂�斾�ׁ@�⍇��          '
source ../job/CRC_LIBRARY.sh
ENDJOB:
