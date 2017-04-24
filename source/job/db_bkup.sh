#!/bin/tcsh
#引数[1]　:実行オプション（-b:バックアップ、-r：リストア）
#引数[2]　:呼び出し元ファイル名
#引数[3～]:作成か削除するファイル名リスト
#戻り値=0 :バックアップ成功
#      !=0:バックアップ失敗
set SYSTEM_DATE=`date '+%Y%m'`
set USER="mysql"
set PASSWORD="mysql"
set DATABASE="testdb"
set FILE_PATH="../tmp"
set ii=3
if (${#argv} > 1) then
  switch (${argv[1]})
    #バックアップ
    case "-b":
      while (${ii} <= ${#argv} )
        if ("${argv[${ii}]}" == "!") then
            mysqldump --user=${USER} --password=${PASSWORD} --all-databases > "${FILE_PATH}/all-file-${SYSTEM_DATE}.sql"
        else
            mysqldump --user=${USER} --password=${PASSWORD} ${DATABASE} ${argv[${ii}]} > "${FILE_PATH}/${argv[${ii}]}-${SYSTEM_DATE}.sql"
        endif
        if ($? == 0) then
          logger -t db_bkup "db_bkup OK Backup:${argv[${ii}]} Parent:${argv[2]}"
        else
          #失敗したらその旨をログに保存
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2定義なし(コマンド実行時)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
    #リストア
    case "-r":
      while (${ii} <= ${#argv} )
        mysql --user=${USER} --password=${PASSWORD} ${DATABASE} < "${FILE_PATH}/${argv[${ii}]}"
        if ($? == 0) then
          logger -t db_bkup "db_bkup OK Backup:${argv[${ii}]} Parent:${argv[2]}"
        else
          #失敗したらその旨をログに保存
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2定義なし(コマンド実行時)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
    #規定外(オプション指定エラー)
    default:
      #オプション指定が無いときはbackupで動く
      @ ii=${ii} - 1
      while (${ii} <= ${#argv} )
        if ("${argv[${ii}]}" == "!") then
            mysqldump --user=${USER} --password=${PASSWORD} --all-databases > "${FILE_PATH}/all-file-${SYSTEM_DATE}.sql"
        else
            mysqldump --user=${USER} --password=${PASSWORD} ${DATABASE} ${argv[${ii}]} > "${FILE_PATH}/${argv[${ii}]}-${SYSTEM_DATE}.sql"
        endif
        if ($? == 0) then
          logger -t db_bkup "db_bkup OK Backup:${argv[${ii}]} Parent:${argv[2]}"
        else
          #失敗したらその旨をログに保存
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2定義なし(コマンド実行時)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
  endsw
else
  #引数エラー
  if (${?FN2} == 1) then
      echo "db_bkup NG argv:Not enough" | tee ${FN2} | logger -t db_bkup 
  else
	  #FN2定義なし(コマンド実行時)
      echo "db_bkup NG argv:Not enough" 
  endif
  exit 1
endif
exit 0