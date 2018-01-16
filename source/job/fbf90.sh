#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
FLCNV 'IDE=NO    ODE=MSD   OFI=FBFT-LOG CMD=BOTH CLR=NO'
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
