#!/bin/tcsh
source ../job/RC_INIT.sh
#**                                                                         00010
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TDIF IGN= IFO=PROTECT ODE=MSD OCI= OFI=O-TDIF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00080
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=TDIRF IGN= IFO=PROTECT ODE=MSD OCI= OFI=O-TDIRF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00150
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=WTNAF IGN= IFO=PROTECT ODE=MSD OCI= OFI=O-WTNAF OGN= LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00220
set ABORT=0;
FLCNV 'IDE=MSD ICI= IFI=JMST1 IGN= IFO=PROTECT ODE=MSD OCI= OFI=O-JMSTD OGN=  LST=NO ERR=ABORT MOD=CREATE AMD=LOGICAL UMD=NO GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         00290
set ABORT=0;
../exec/SETTDI $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
