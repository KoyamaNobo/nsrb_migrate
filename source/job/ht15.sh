#!/bin/tcsh
source ../job/RC_INIT.sh
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊　　　　得意先別　売掛データ　変換　　　　　＊   '
echo '                 ＊                                            ＊   '
echo '                 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊   '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                                                                    '
echo '                       実行する     : 　　CTRL + F5 を押下          '
echo '                     　実行しない　 : 　　CTRL + F9 を押下          '
f5orf9 ; if ($status == 1) exit 1;
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TUKOF IGN= IFO=PROTECT ODE=MSD OCI= OFI=TUKFW OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TUKF1 IGN= IFO=PROTECT ODE=MSD OCI= OFI=TUKFW OGN= LST=NO ERR=ABORT MOD=ADD AMD=LOGICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
