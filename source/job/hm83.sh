#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                   ����������������������������������������������   '
echo '                   ���@�@�@�l�r�c�Z�[�u��@�]���ւ������@�@�@�@��   '
echo '                   ����������������������������������������������   '
echo '                                                                    '
echo '                   �g�h�l�@�U�֒P���ύX                             '
echo '                   �g�t�g�l�@�O�c�E���J���z�ύX                     '
echo '                   �e�ݐσt�@�C���@�U�֋��z�ăZ�b�g                 '
echo '                                                                    '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
source ../job/db_bkup.sh "hm83.sh" "\!"
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
../exec/HMM770 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
