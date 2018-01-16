#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMY720 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0256' '(1,4,N)' '((1,4),(11,7),(18,10),(28,10),(@          @),(1,64),(1,64),(' '1,64),(1,23))' '((11,7),(18,10),(28,10))' '' '((42,2,N,GE,@11@)A(42,2,N,LE,@13@))' '' '        出荷集計年間累積Ｆ　集計        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '(12,10,S,D)' '' '' '' '' '' '' '        得意先別年間順位Ｆ　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '(1,10,N)' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10),(28,10))' '' '((42,2,N,GE,@11@)A(42,2,N,LE,@13@))' '' ' Ｃ地下得意先順位別　年間売上粗利集計表 '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY710 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0256' '(1,4,N)' '((1,4),(11,7),(18,10),(28,10),(@          @),(1,64),(1,64),(' '1,64),(1,23))' '((11,7),(18,10),(28,10))' '' '(42,2,N,EQ,@81@)' '' '        出荷集計年間累積Ｆ　集計        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '(12,10,S,D)' '' '' '' '' '' '' '        得意先別年間順位Ｆ　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '(1,10,N)' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10),(28,10))' '' '(42,2,N,EQ,@81@)' '' ' Ｃ長靴得意先順位別　年間売上粗利集計表 '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY710 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0256' '(1,4,N)' '((1,4),(11,7),(18,10),(28,10),(@          @),(1,64),(1,64),(' '1,64),(1,23))' '((11,7),(18,10),(28,10))' '' '((42,2,N,GE,@41@)A(42,2,N,LE,@42@))' '' '        出荷集計年間累積Ｆ　集計        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '(12,10,S,D)' '' '' '' '' '' '' '        得意先別年間順位Ｆ　作成        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '(1,8,N)' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10),(28,10))' '' '((42,2,N,GE,@41@)A(42,2,N,LE,@42@))' '' ' Ｃ作業得意先順位別　年間売上粗利集計表 '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY710 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#set ABORT=0;
#set JRCODE=000;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '((40,2,N),(1,10,N))' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10),(28,10))' '' '(46,2,N,EQ,@30@)' '' '        出荷集計年間累積Ｆ　集計        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMY380 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0064' 'WK0128' '(5,4,N)' '((1,64),(@          @),(1,54))' '' '((11,7),(18,10))' '' '(46,2,N,EQ,@30@)' '' '       教育品種別　年間売上集計表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY370 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
