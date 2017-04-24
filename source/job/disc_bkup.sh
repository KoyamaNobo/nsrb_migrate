#!/bin/tcsh
#����[1]�@:���s�I�v�V�����i-b:�o�b�N�A�b�v�A-r�F���X�g�A�j
#����[2]�@:���s�I�v�V�����i-f:�t�@�C���w��A-h�F�q�X�g���w��j
#����[3]�@:�t�@�C�����܂��̓q�X�g��(yyyymm)�i���C���h�J�[�h�g�p�j
#�߂�l=0 :�������s����
#      !=0:�������s���s

set SYSTEM_DATE=`date '+%Y%m'`
set USER="mysql"
set PASSWORD="mysql"
set DATABASE="testdb"
set FILE_PATH="../tmp"
set DISC_PATH="/mnt"

set inFile=""
set existDir=""
set existFile=""
set errcont=""
set errtag=""
set errf=0
set cnt=0

#�����`�F�b�N�A���̓t�@�C�����ҏW
if (${#argv} <= 3) then
  #�������[�h
  switch (${argv[1]})
    #�o�b�N�A�b�v
    case "-b"
      set inFile=${FILE_PATH}
      breaksw
    #���X�g�A
    case "-r"
      set inFile=${DISC_PATH}


#      set inFile="../tmp/kawamnt"


      breaksw
    #�K��O(�I�v�V�����w��G���[)
    default:
      set errcont="invalid exec. options"
      set errtag="xxxxx"
      set errf=1
      breaksw
  endsw
  if (${errf} == 0) then
    #�t�@�C���w��
    switch (${argv[2]})
      #�t�@�C���w��
      case "-f"
        set inFile="${inFile}/${argv[3]}"
        breaksw
      #�q�X�g���w��
      case "-h"
        set inFile="${inFile}/*-${argv[3]}.sql"
        breaksw
      #�K��O(�I�v�V�����w��G���[)
      default:
        set errcont="invalid file select options"
        set errtag="xxxxx"
        set errf=1
        breaksw
    endsw
  endif
else
  #�����G���[
  set errcont="Not enough"
  set errtag="xxxxx"
  set errf=1
endif

#�����f�B���N�g���̊m�F
if (${errf} == 0) then
  set existDir=${FILE_PATH}
  if (! -e ${existDir}) then
    mkdir -m 777 ${existDir}
    if ($? != 0) then
      #�����f�B���N�g���쐬���s
      set errcont="Temporary-directory make failed"
      set errtag="mkdir"
      set errf=1
    endif
  endif
  if (! -d ${existDir}) then
    #�����f�B���N�g���Ȃ�
      set errcont="can't find a Temporary-directory"
      set errtag="xxxxx"
      set errf=1
  endif
endif

#�w��ɂ��e�����ɕ���
if (${errf} == 0) then
  if ("${argv[1]}" == "-b") then
    #�o�b�N�A�b�v����
    #���̓t�@�C������iso�t�@�C�����쐬
    genisoimage -quiet -r -J -o ${existDir}/discwrite.iso ${inFile}
    if ($? == 0) then
      set existFile="${existDir}/discwrite.iso"
      if (-e ${existFile}) then
#        echo "�o�b�N�A�b�v�t�@�C�����f�B�X�N�ɏ����o���܂��B"
#        echo "�������ł����牽���L�[�������Ă��������B"
        echo "I've done all of my preparing for backup-files to backup-disc,"
        echo "Please press enter key to continue..."
        set rd=$<
        #�f�B�X�N�̏���
        sudo wodim dev=/dev/scd0 -blank=fast
        if ($? != 0) then
          #�f�B�X�N�����G���[
          set errcont="wodim exec.(erase) error"
          set errtag="wodim"
          set errf=1
        else
          #iso�t�@�C�����f�B�X�N�ɏ����o��
          set cnt=0
          sudo wodim -nofix dev=/dev/scd0 -data "${FILE_PATH}/discwrite.iso"
          if ($? != 0) then
            #�f�B�X�N�������݃G���[
            set errcont="wodim exec.(write) error"
            set errtag="wodim"
            set errf=1
          else
            sleep 180s
            sudo wodim -eject dev=/dev/scd0
            while ($? != 0)
              sleep 3s
              sudo wodim -eject dev=/dev/scd0
              @ cnt+=1
              if (${cnt} >= 10) then
                break
              endif
            end
          endif
        endif
      else
        #iso�t�@�C���Ȃ�
        set errcont="can't find a iso-files"
        set errtag="wodim"
        set errf=1
      endif
    else
      #iso�쐬�G���[
      set errcont="genisoimage exec. error"
      set errtag="genisoimage"
      set errf=1
    endif
  else
    #���X�g�A����
#    echo "�f�B�X�N����o�b�N�A�b�v�t�@�C�����擾���܂��B"
#    echo "�������ł����牽���L�[�������Ă��������B"
    echo "I've done all of my preparing for get a backup-files from backup-disc,"
    echo "Please press enter key to continue..."
    set rd=$<
    #�f�B�X�N�h���C�u���}�E���g
    sudo mount /dev/scd0 ${DISC_PATH}


#sudo mount -t iso9660 -o loop ../tmp/discwrite.iso "${FILE_PATH}/kawamnt"


    #�w��t�@�C�����o�b�N�A�b�v�f�B���N�g���ɕ���
    cp -f ${inFile} "${existDir}/."
    #�f�B�X�N�h���C�u���A���}�E���g
    set cnt=0
    sudo umount ${DISC_PATH}
    sudo wodim -eject dev=/dev/scd0
    do while ($? != 0)
      sleep 3s
      sudo wodim -eject dev=/dev/scd0
      @ cnt+=1
      if (${cnt} >= 10) then
        break
      endif
    end


#sudo umount "${FILE_PATH}/kawamnt"


  endif
endif

#�I������(���s����)
if (${errf} == 0) then
  logger -t disc_bkup "disc_bkup OK"
  echo "disc_bkup OK"
  exit 0
else
  if (${?FN2} == 1) then
    echo "disc_bkup NG:${errcont}" | tee ${FN2} | logger -t "${errtag}" 
  else
    #FN2��`�Ȃ�(�R�}���h���s��)
    echo "disc_bkup NG:${errcont}" 
  endif
  exit 1
endif
