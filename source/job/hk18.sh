#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0064' '((54,8,N),(1,4,N))' '((54,8),(1,4),(186,1),(@          @),(1,41))' '' '(186,1)' '' '((54,8,N,NE,@00000000@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@' '000000@))' '        �������@���s�\��@�⍇��        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG830 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
