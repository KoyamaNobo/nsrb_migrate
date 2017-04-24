#!/bin/tcsh
#引数[1]　:呼び出し元ファイル名
#引数[2～]:TRUNCATEするファイル名リスト
#戻り値=0 :TRUNCATE成功
#      !=0:TRUNCATE失敗

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
      #失敗したらその旨をログに保存
      echo "Alloc NG truncate:${argv[${ii}]} Parent:${argv[1]}" | tee ${FN2} | logger -t mysqldump 
      exit 1
    endif
    @ ii++
  end
else
  #引数エラー
  echo "Alloc NG argv:Not enough" | tee ${FN2} | logger -t mysqldump 
  exit 1
endif

exit 0
