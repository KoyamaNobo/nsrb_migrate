#***@ eSŌ@P[i`Sj @
#/SET JRCODE=000;                                                           
#*** @ Ð@@@· @ 
#***  =====  ŠÞĖĀŅ\EŧióĨ\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(78,10))' '' '' '        ŠÞ@Wvt@C@ėŽ        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞĖĀŅ\ ---                                                
#***  INPUT  : DATEM,HKBM,WK0128NNN                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞŧióĨ\ ---                                                
#***  INPUT  : DATEM,HKBM,WK0128NNN                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  HipræŠĘ ĖĀŅ\  =====                               
#***  INPUT  : DATEM,KKBM,KHM,KHTM2                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  HipræŠĘ ŧióĨ\  =====                               
#***  INPUT  : DATEM,KKBM,KHM,KHTM2                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  åĘãÎä\  ==========                                 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE 'IDE=MSD ICI= IFI=TZNTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=TZNTPM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ūÓæNÔĖ[N XV ---                                     
#***  INPUT  : DATEM,TTM,SNTRF                                              
#***  I-O    : TZNTPM                                                       
set ABORT=0;
../exec/HMG990 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ūÓæNÔĖ[N Wv ---                                     
#***  INPUT  : TZNTPM                                                       
#***  OUTPUT : WK0512NNN                                                    
set ABORT=0;
../exec/HMY170 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  SūÓæNÔĘ@ãEeÎä\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- åĘ ãÎä\ ---                                           
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 0 0 1 2 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãeWv\  =====                            
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      SūÓæĘ@ãeWv\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãeWv\ ---                                   
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ Ž@@@ė @ 
#***  =====  SūÓæižĘ  ãeWv\iģįj  =====                
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '   ūÓæiíĘ ãeWv\ (ģį)   ' 
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\iģįj ---                       
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 80 89 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\iģįj  =====                      
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@2@)' '' '  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\iģįj ---                             
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 2 0 1 0 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 80 89 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  NÔūÓæižTCYĘ@oŨūŨ\iģįj@=====            
#***  --- NÔižTCYĘf[^@oiæŠj ---                        
#***  INPUT  : SNTRF,STRANYR                                                
#***  I-O    : DATEM                                                        
#***  OUTPUT : WK0064                                                       
set ABORT=0;
../exec/HMY060 $USER_ID $JRCODE 1 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'WK0064' 'WK0064' '(1,7,N)' '' '' '((8,4,P),(12,4,P),(16,4,P),(20,4,P),(24,4,P),(28,4,P),(32,4,P),(36,4,P),(40,4,P),(44,4,P),(48,5,P))' '' '' '     ģįižTCYĘ@oŨūŨ\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- NÔižTCYĘf[^@oiæŠģįj ---                    
#***  INPUT  : DATEM,HIM1,WK0064NNN                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY080 $USER_ID $JRCODE 1 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ O@@@c @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæŠÞĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 7 00 99 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '11' 'WK0064' 'SHA' 'WK0128' 'CRE' '' '' '' '' '(13,4,N,EQ,@1522@)O(13,4,N,EQ,@1528@)O(13,4,N,EQ,@1541@)O(13,4,N,EQ,@1542@)O(13,4,N,EQ,@1544@)' '    SūÓæiíĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((50,2,N),(9,4,N),(13,6,N))' '(1,64)' '' '' '' '' '' '    SūÓæiíĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ Ž@@@Ņ @ięĘj@ 
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 01 09 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 4 01 09 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ŠÞTCYĘ@ÝÉūŨ\  =====                                
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            Ļ@ÝÉūŨ\              '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞTCYĘ ÝÉūŨ\ ---                                       
#***  INPUT  : DATEM,HIM,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMD620 $USER_ID $JRCODE 0 00 99 0 9 260 269 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ R@@@č @ 
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 4 10 19 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 80 89 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  SūÓæNÔĘ@ãEeÎä\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 10 19 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ŠÞižĘ@ŧióĨ\iĐŠĖÔj  =====                    
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           Ļ@iíĘóĨ\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞižĘŧióĨ\ ---                                          
#***  INPUT  : DATEM,HKBM,HIM1,WK0128NNN                                    
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 1 0 0 2000 2080 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ ā@@@c @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæŠÞĘ@ãeWv\      '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 6 20 29 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    SūÓæiíĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 1 20 29 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 20 29 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**           uQlŋv                                                  
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ŠÞ@Wvt@C@ėŽ        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
SET JRCODE=010;
set ABORT=0;
../exec/HMG000 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'00' \
'WK0128' \
'((92,2,N),(96,1,N),(88,3,N),(1,6,N))' \
'' \
'' \
'' \
'' \
'' \
'' \
'           Ļ@iíĘóĨ\               '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 0 0 1 0 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
../exec/HMG000 $USER_ID $JRCODE 2
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'22' \
'WK0128' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'      SūÓæĘ@ãeWv\          '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
#*** @ a@@@c @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(85,2,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæŠÞĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 6 30 39 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 30 39 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'TZNTPM' \
'WK0512' \
'((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' \
'' \
'' \
'' \
'' \
'(5,1,N,EQ,@1@)' \
'' \
'  SūÓæNÔĘ@ãEeÎä\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 30 39 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ Â@@@ä @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(85,2,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæŠÞĘ@ãeWv\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 6 40 49 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 40 49 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'TZNTPM' \
'WK0512' \
'((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' \
'' \
'' \
'' \
'' \
'(5,1,N,EQ,@1@)' \
'' \
'  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 40 49 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ â@@@c @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(85,2,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæŠÞĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 6 50 59 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 50 59 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'TZNTPM' \
'WK0512' \
'((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' \
'' \
'' \
'' \
'' \
'(5,1,N,EQ,@1@)' \
'' \
'  SūÓæNÔĘ@ãEeÎä\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 50 59 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  nCp[uSūÓæiíĘãWv\  =====                    
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  ---  nCp[uSūÓæiíĘãWv\  ---                        
#***  INPUT  : DATEM,TM1,HIM,WK0064NNN                                      
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG020 $USER_ID $JRCODE 00 99 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ @@@ @ 
#***  =====  SūÓæŠÞĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(85,2,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæŠÞĘ@ãeWv\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæŠÞĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,HKBM,WK0064NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG030 $USER_ID $JRCODE 6 60 69 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 60 69 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'TZNTPM' \
'WK0512' \
'((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' \
'' \
'' \
'' \
'' \
'(5,1,N,EQ,@1@)' \
'' \
'  SūÓæNÔĘ@ãEeÎä\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\ ---                                     
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 1 0 1 0 60 69 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ iBFfBj @ 
#***  =====  SūÓæižĘ  ãeWv\iBFfBj  =====      
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'ūÓæiíĘ ãeWv\ (ģÞĻģÞŠÝÃÞĻ)'
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\iBFfBj ---             
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæižĘ  ãeWv\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SNTRF' \
'WK0064' \
'((92,2,N),(16,4,N),(20,6,N))' \
'((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' \
'23,2),(125,1),(@      @))' \
'' \
'' \
'(7,1,N,NE,@9@)' \
'' \
'    SūÓæiíĘ@ãeWv\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæižĘ ãeWv\ ---                               
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG010 $USER_ID $JRCODE 1 06 06 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ŋūŨ\  =====                                
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SMF' \
'WK0128' \
'((93,2,N),(1,4,N),(5,8,N),(83,6,N))' \
'((1,102),(@          @),(1,16))' \
'' \
'' \
'' \
'' \
'' \
'        SūÓæĘ@ŋūŨ\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ŋūŨ\ ---                                       
#***  INPUT  : DATEM,TM1,TTM,WK0128NNN                                      
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HKG060 $USER_ID $JRCODE 2 06 06 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæútĘ@üāūŨ\  =====                            
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'NYURF' \
'WK0128' \
'((39,2,N),(9,4,N),(1,8,N),(21,2,N),(24,8,N))' \
'((1,102),(@          @),(1,16))' \
'' \
'' \
'' \
'' \
'' \
'        SūÓæĘ@üāūŨ\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæútĘ üāūŨ\ ---                                   
#***  INPUT  : DATEM,TM1,HKBM,WK0128NNN                                     
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HKG320 $USER_ID $JRCODE 06 06 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ižTCYĘ@óĨ\iBFfBj  =====                  
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'HHTF1' \
'WK0256' \
'((248,2,N),(250,1,N),(244,3,N),(7,6,N))' \
'' \
'' \
'' \
'' \
'' \
'' \
'          ižTCYĘ@óĨ\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ižTCYĘ óĨ\ ---                                           
#***  INPUT  : DATEM,HIM1,WK0256                                            
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG720 $USER_ID $JRCODE 00 99 0 9 322 322 1 
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  SūÓæĘ@ãÎä\iBFfBj  =====            
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'TZNTPM' \
'WK0512' \
'((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' \
'' \
'' \
'' \
'' \
'(5,1,N,EQ,@3@)' \
'' \
'  SūÓæNÔĘ@ãEeÎä\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ãÎä\iBFfBj ---                   
#***  INPUT  : DATEM,TM1,WK0512NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMY160 $USER_ID $JRCODE 1 3 0 1 0 1 1 9 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ @@V@@ @ 
#***  =====  åĘúvWv\  =====                                        
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,URIRF                                  
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HKG400 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  Ļúv\  =====                                              
#***  INPUT  : DATEM,TTM,SNTRF,NYURF,UTRF,STRAN,NYUF,UTRAN                  
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMD710 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  Hižúv\  =====                                            
#***  INPUT  : DATEM,TTM,CALNM,URIRF,NYURF,BUMON-K,URIF,NYUF                
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHD610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  ŠÞĖĀŅ\EŧióĨ\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'HUHM' \
'WK0128' \
'((92,2,N),(96,1,N),(88,2,N))' \
'((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' \
'10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' \
'((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' \
'78,10))' \
'' \
'' \
'        ŠÞ@Wvt@C@ėŽ         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞĖĀŅ\ ---                                                
#***  INPUT  : DATEM,HKBM,WK0128NNN                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG240 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞŧióĨ\ ---                                                
#***  INPUT  : DATEM,HKBM,WK0128NNN                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMG250 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  HipræŠĘ ĖĀŅ\  =====                               
#***  INPUT  : DATEM,KKBM,KHM,KHTM2                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHG080 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  HipræŠĘ ŧióĨ\  =====                               
#***  INPUT  : DATEM,KKBM,KHM,KHTM2                                         
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  =====  åūÓæĘ@ŋcūŨ\  =====                            
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SKDF' \
'WK0512' \
'(54,8,N)' \
'((54,8),(@          @),(186,1),(1,192),(1,192),(1,109))' \
'' \
'(186,1)' \
'' \
'((54,8,N,NE,@00000000@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@' \
'000000@))' \
'            ŋc@ūŨ\              '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'SKDF' \
'WK0256' \
'(1,20,C)' \
'((1,192),(@          @),(1,54))' \
'' \
'' \
'' \
'((13,1,N,NE,@5@)A(54,8,N,NE,@99999999@)A(187,6,N,EQ,@000000@' \
'))' \
'            ŋc@ūŨ\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŋ|cf[^ o ---                                       
#***  INPUT  : DATEM,TM1,TSKF,WK0256NNN,WK0512NNN                           
#***  OUTPUT : WK0064NNN                                                    
set ABORT=0;
../exec/HKG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'00' \
'WK0064' \
'(1,4,N)' \
'' \
'' \
'((5,9),(14,8),(22,9),(31,9),(40,7))' \
'' \
'' \
'' \
'            ŋc@ūŨ\            '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**                                                                         
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'00' \
'WK0064' \
'((49,1,N),(1,4,N))' \
'' \
'' \
'' \
'' \
'' \
'' \
'   åĘ  ŋc@ūŨ\  (op)    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- SūÓæĘ ŋcūŨ\ ---                                   
#***  INPUT  : DATEM,TM1,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HKG120 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ @@@ž @ 
#***  =====  ŠÞTCYĘ@ÝÉūŨ\  =====                                
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'00' \
'WK0128' \
'((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' \
'' \
'' \
'' \
'' \
'' \
'' \
'            Ļ@ÝÉūŨ\              '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ŠÞTCYĘ ÝÉūŨ\ ---                                       
#***  INPUT  : DATEM,HIM,WK0064NNN                                          
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/HMD620 $USER_ID $JRCODE 0 00 99 0 9 260 269 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ c@š@ @ 
#***  =====  HiÞŋ@düúv\@=====                                    
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'KSRF' \
'WK0064' \
'((64,1,N),(1,8,N),(9,7,C))' \
'' \
'' \
'' \
'' \
'' \
'' \
'          HiÞŋ@düúv\            '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- HiÞŋ düúv\ ---                                           
#***  INPUT  : DATEM,KKBM1,KJM,WK0064NNN                                    
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KBK110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#*** @ Â   ä  ūÞ@ 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'URIRF' \
'WK0128' \
'((10,4,N),(43,2,N),(14,5,C),(55,1,N),(2,8,N))' \
'((1,85),(@          @),(1,33))' \
'' \
'' \
'' \
'' \
'' \
'          CS@ãÆ\          '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- CS ãÆ\ ---                                           
#***  INPUT  : DATEM,KHM,TTM,WK0128NNN                                      
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KHG410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***                                                                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE \
'10' \
'KSRF' \
'WK0064' \
'((64,1,N),(9,7,C),(1,8,N))' \
'' \
'' \
'' \
'' \
'' \
'' \
'        CS@dü@Æ\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- CS düÆ\ ---                                           
#***  INPUT  : DATEM,KKBM,STM,KJM,WK0128NNN                                 
#***  OUTPUT : PRN999                                                       
set ABORT=0;
../exec/KBK010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
