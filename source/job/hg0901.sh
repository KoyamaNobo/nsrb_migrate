#***�����������������@ �e�S���ҁ@�P�[�i�`�S�j �@����������������������������
#/SET JRCODE=000;                                                           
#*** �����������������@ �Ё@�@�@�� �@�������������������������������������� 
#***  =====  ���ޔ̔����ѕ\�E���i�󕥕\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޔ̔����ѕ\ ---                                                
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
#***  --- ���ސ��i�󕥕\ ---                                                
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
#***  =====  �H�i�p�r�敪�� �̔����ѕ\  =====                               
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
#***  =====  �H�i�p�r�敪�� ���i�󕥕\  =====                               
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
#***  =====  ���匎�ʔ���Δ�\  ==========                                 
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE 'IDE=MSD ICI= IFI=TZNTM IGN= IFO=PROTECT ODE=MSD OCI= OFI=TZNTPM OGN= LST=NO ERR=ABORT MOD=CREATE AMD=PHYSICAL CMD=BOTH GTR= DEL=NO CLR=NO OUT= RNG= SEL= SAV= NXT= '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���Ӑ�N�Ԕ̔����[�N �X�V ---                                     
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
#***  --- ���Ӑ�N�Ԕ̔����[�N �W�v ---                                     
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���匎�� ����Δ�\ ---                                           
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
#***  =====  �S�����Ӑ�ʁ@����e���W�v�\  =====                            
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '      �S�����Ӑ�ʁ@����e���W�v�\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� ����e���W�v�\ ---                                   
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
#*** �����������������@ ���@�@�@�� �@�������������������������������������� 
#***  =====  �S�����Ӑ�i����  ����e���W�v�\�i����j  =====                
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '   ���Ӑ�i��� ����e���W�v�\ (����)   ' 
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\�i����j ---                       
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\�i����j  =====                      
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@2@)' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\�i����j ---                             
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#***  =====  �N�ԓ��Ӑ�i���T�C�Y�ʁ@�o�א����ו\�i����j�@=====            
#***  --- �N�ԕi���T�C�Y�ʃf�[�^�@���o�i�挎���j ---                        
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
../exec/CSRT50 $USER_ID $JRCODE '10' 'WK0064' 'WK0064' '(1,7,N)' '' '' '((8,4,P),(12,4,P),(16,4,P),(20,4,P),(24,4,P),(28,4,P),(32,4,P),(36,4,P),(40,4,P),(44,4,P),(48,5,P))' '' '' '     ����i���T�C�Y�ʁ@�o�א����ו\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �N�ԕi���T�C�Y�ʃf�[�^�@���o�i�挎������j ---                    
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
#*** �����������������@ �O�@�@�@�c �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ敪�ޕʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(123,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '11' 'WK0064' 'SHA' 'WK0128' 'CRE' '' '' '' '' '(13,4,N,EQ,@1522@)O(13,4,N,EQ,@1528@)O(13,4,N,EQ,@1541@)O(13,4,N,EQ,@1542@)O(13,4,N,EQ,@1544@)' '    �S�����Ӑ�i��ʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0128' 'WK0064' '((50,2,N),(9,4,N),(13,6,N))' '(1,64)' '' '' '' '' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\     '
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
#*** ���������������@ ���@�@�@�� �@�i��ʁj�@������������������������������ 
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  ���ރT�C�Y�ʁ@�݌ɖ��ו\  =====                                
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '            �����@�݌ɖ��ו\              '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ރT�C�Y�� �݌ɖ��ו\ ---                                       
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
#*** �����������������@ �R�@�@�@�� �@�������������������������������������� 
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#***  =====  ���ޕi���ʁ@���i�󕥕\�i�����̎��ԁj  =====                    
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HUHM' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '           �����@�i��ʎ󕥕\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޕi���ʐ��i�󕥕\ ---                                          
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
#*** �����������������@ ���@�@�@�c �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(85,2,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ敪�ޕʁ@����e���W�v�\      '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((92,2,N),(16,4,N),(20,6,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '    �S�����Ӑ�i��ʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TZNTPM' 'WK0512' '((484,2,N),(222,10,S,D),(232,10,S,D),(1,4,N))' '' '' '' '' '(5,1,N,EQ,@1@)' '' '  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#**           �u�Q�l�����v                                                  
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
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((92,2,N),(96,1,N),(88,2,N))' '((88,9),(@0@),(13,6),(@0@),(19,9),(28,7),(35,10),(46,7),(53,' '10),(@0@),(63,6),(@0@),(69,9),(78,10),(@     @),(1,36))' '((13,6),(19,9),(28,7),(35,10),(46,7),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        ���ށ@�W�v�t�@�C���@�쐬        '
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
'           �����@�i��ʎ󕥕\               '
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
'      �S�����Ӑ�ʁ@����e���W�v�\          '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMG180 $USER_ID $JRCODE 2 00 99 1
source ../job/CRC_LIBRARY.sh
#*** �����������������@ �a�@�@�@�c �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
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
'    �S�����Ӑ敪�ޕʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
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
'  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#*** �����������������@ �@�@�@�� �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
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
'    �S�����Ӑ敪�ޕʁ@����e���W�v�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
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
'  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#*** �����������������@ ��@�@�@�c �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
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
'    �S�����Ӑ敪�ޕʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
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
'  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#***  =====  �n�C�p�[�u�S�����Ӑ�i��ʔ���W�v�\  =====                    
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  ---  �n�C�p�[�u�S�����Ӑ�i��ʔ���W�v�\  ---                        
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
#*** �����������������@ ���@�@�@�� �@�������������������������������������� 
#***  =====  �S�����Ӑ敪�ޕ�  ����e���W�v�\  =====                        
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
'    �S�����Ӑ敪�ޕʁ@����e���W�v�\    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ敪�ޕ� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\  =====                              
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
'  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\  '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\ ---                                     
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
#*** �����������������@ �����i���B���F���f�B�j �@�������������������������� 
#***  =====  �S�����Ӑ�i����  ����e���W�v�\�i���B���F���f�B�j  =====      
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
'���Ӑ�i��� ����e���W�v�\ (�ި�ު��ި)'
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\�i���B���F���f�B�j ---             
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
#***  =====  �S�����Ӑ�i����  ����e���W�v�\  =====                        
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
'    �S�����Ӑ�i��ʁ@����e���W�v�\     '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�i���� ����e���W�v�\ ---                               
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
#***  =====  �S�����Ӑ�ʁ@�������ו\  =====                                
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
'        �S�����Ӑ�ʁ@�������ו\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� �������ו\ ---                                       
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
#***  =====  �S�����Ӑ���t�ʁ@�������ו\  =====                            
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
'        �S�����Ӑ�ʁ@�������ו\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ���t�� �������ו\ ---                                   
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
#***  =====  �i���T�C�Y�ʁ@�󕥕\�i���B���F���f�B�j  =====                  
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
'          �i���T�C�Y�ʁ@�󕥕\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �i���T�C�Y�� �󕥕\ ---                                           
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
#***  =====  �S�����Ӑ挎�ʁ@����Δ�\�i���B���F���f�B�j  =====            
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
'  �S�����Ӑ�N�Ԍ��ʁ@����E�e���Δ�\   '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ挎�� ����Δ�\�i���B���F���f�B�j ---                   
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
#*** �����������������@ �@�@�V�@�@ �@�������������������������������������� 
#***  =====  ����ʓ��v�W�v�\  =====                                        
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
#***  =====  �������v�\  =====                                              
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
#***  =====  �H�i�����v�\  =====                                            
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
#***  =====  ���ޔ̔����ѕ\�E���i�󕥕\  =====                              
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
'        ���ށ@�W�v�t�@�C���@�쐬         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ޔ̔����ѕ\ ---                                                
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
#***  --- ���ސ��i�󕥕\ ---                                                
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
#***  =====  �H�i�p�r�敪�� �̔����ѕ\  =====                               
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
#***  =====  �H�i�p�r�敪�� ���i�󕥕\  =====                               
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
#***  =====  ���哾�Ӑ�ʁ@�����c�����ו\  =====                            
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
'            �����c���@���ו\              '
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
'            �����c���@���ו\           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �������|�c���f�[�^ ���o ---                                       
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
'            �����c���@���ו\            '
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
'   �����  �����c���@���ו\  (�o���p)    '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �S�����Ӑ�� �����c�����ו\ ---                                   
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
#*** ���������������@ ���@�@�@�� �@���������������������������������������� 
#***  =====  ���ރT�C�Y�ʁ@�݌ɖ��ו\  =====                                
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
'            �����@�݌ɖ��ו\              '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���ރT�C�Y�� �݌ɖ��ו\ ---                                       
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
#*** �����������������@ �c�@���@�� �@�������������������������������������� 
#***  =====  �H�i�ޗ��@�d�����v�\�@=====                                    
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
'          �H�i�ޗ��@�d�����v�\            '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- �H�i�ޗ� �d�����v�\ ---                                           
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
#*** �����������������@ ��   ��  �ށ@�������������������������������������� 
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
'          ���C�S���@����ƍ��\          '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���C�S�� ����ƍ��\ ---                                           
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
'        ���C�S���@�d�����@�ƍ��\         '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***  --- ���C�S�� �d���ƍ��\ ---                                           
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
