#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
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
