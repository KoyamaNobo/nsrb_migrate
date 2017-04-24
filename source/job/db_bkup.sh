#!/bin/tcsh
#����[1]�@:���s�I�v�V�����i-b:�o�b�N�A�b�v�A-r�F���X�g�A�j
#����[2]�@:�Ăяo�����t�@�C����
#����[3�`]:�쐬���폜����t�@�C�������X�g
#�߂�l=0 :�o�b�N�A�b�v����
#      !=0:�o�b�N�A�b�v���s
set SYSTEM_DATE=`date '+%Y%m'`
set USER="mysql"
set PASSWORD="mysql"
set DATABASE="testdb"
set FILE_PATH="../tmp"
set ii=3
if (${#argv} > 1) then
  switch (${argv[1]})
    #�o�b�N�A�b�v
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
          #���s�����炻�̎|�����O�ɕۑ�
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2��`�Ȃ�(�R�}���h���s��)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
    #���X�g�A
    case "-r":
      while (${ii} <= ${#argv} )
        mysql --user=${USER} --password=${PASSWORD} ${DATABASE} < "${FILE_PATH}/${argv[${ii}]}"
        if ($? == 0) then
          logger -t db_bkup "db_bkup OK Backup:${argv[${ii}]} Parent:${argv[2]}"
        else
          #���s�����炻�̎|�����O�ɕۑ�
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2��`�Ȃ�(�R�}���h���s��)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
    #�K��O(�I�v�V�����w��G���[)
    default:
      #�I�v�V�����w�肪�����Ƃ���backup�œ���
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
          #���s�����炻�̎|�����O�ɕۑ�
          if (${?FN2} == 1) then
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | tee ${FN2} | logger -t db_bkup 
          else
              #FN2��`�Ȃ�(�R�}���h���s��)
              echo "db_bkup NG Backup:${argv[${ii}]} Parent:${argv[2]}" | logger -t db_bkup 
          endif
          exit 1
        endif
        @ ii++
      end
      breaksw
  endsw
else
  #�����G���[
  if (${?FN2} == 1) then
      echo "db_bkup NG argv:Not enough" | tee ${FN2} | logger -t db_bkup 
  else
	  #FN2��`�Ȃ�(�R�}���h���s��)
      echo "db_bkup NG argv:Not enough" 
  endif
  exit 1
endif
exit 0