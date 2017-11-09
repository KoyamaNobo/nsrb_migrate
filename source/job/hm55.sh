#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
echo '                                                                    '
echo '                 ������������������������������������������������   '
echo '                 ��                                            ��   '
echo '                 ���@�@�@�@�����U�֒P���@�C���X�V�@�@�@�@�@�@�@��   '
echo '                 ��                                            ��   '
echo '                 ������������������������������������������������   '
echo '                                                                    '
echo '                                                                    '
echo '                       ���s����     : �@�@CTRL + F5 ������          '
echo '                     �@���s���Ȃ��@ : �@�@CTRL + F9 ������          '
f5orf9 ; if ($status == 1) exit 1;
set JRCODE=000;
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HIM1 IGN= IFO=SHARE ODE=MSD OCI= OFI=BU-HIM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HUHM IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-HUHM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-TTM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=STRANYR IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-STRANYR OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=SNTRF IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-SNTRF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=UTRYR IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-UTRYR OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=DATA GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=UTRF IGN= IFO=PROTECT ODE=MSD OCI= OFI=BU-UTRF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM750 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
ENDJOB:
