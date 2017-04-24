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
FLCNV 'IDE=MSD ICI= IFI=HHTF1 IGN= IFO=SHARE ODE=MSD OCI= OFI=HGF-HHTF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HUHM IGN= IFO=SHARE ODE=MSD OCI= OFI=HGF-HUHM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMN710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                    
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HHTF1 IGN= IFO=SHARE ODE=MSD OCI= OFI=HGR-HHTF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=HUHM IGN= IFO=SHARE ODE=MSD OCI= OFI=HGR-HUHM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT='
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
