#!/bin/tcsh
#����[1]�@:�Ăяo�����t�@�C����
#����[2�`]:TRUNCATE����t�@�C�������X�g
#�߂�l=0 :TRUNCATE����
#      !=0:TRUNCATE���s

set SYSTEM_DATE=`date '+%Y%m'`
set USER="mysql"
set PASSWORD="mysql"
set DATABASE="testdb"
set FILE_PATH="./"
set ii=2

if (${#argv} > 1) then
  while (${ii} <= ${#argv} )
    mysql --user=${USER} --password=${PASSWORD} --database=${DATABASE} -e "TRUNCATE TABLE ${argv[${ii}]};"
    
    if ($? == 0) then
      logger -t mysqldump "Alloc OK truncate:${argv[${ii}]} Parent:${argv[1]}"
      #echo "Alloc OK truncate:${argv[${ii}]} Parent:${argv[1]}"
    else
      #���s�����炻�̎|�����O�ɕۑ�
      echo "Alloc NG truncate:${argv[${ii}]} Parent:${argv[1]}" | tee ${FN2} | logger -t mysqldump 
      exit 1
    endif
    @ ii++
  end
else
  #�����G���[
  echo "Alloc NG argv:Not enough" | tee ${FN2} | logger -t mysqldump 
  exit 1
endif

exit 0
