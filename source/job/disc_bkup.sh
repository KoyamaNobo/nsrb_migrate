#!/bin/tcsh
#引数[1]　:実行オプション（-b:バックアップ、-r：リストア）
#引数[2]　:実行オプション（-f:ファイル指定、-h：ヒストリ指定）
#引数[3]　:ファイル名またはヒストリ(yyyymm)（ワイルドカード使用可）
#戻り値=0 :処理実行成功
#      !=0:処理実行失敗

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

#引数チェック、入力ファイル名編集
if (${#argv} <= 3) then
  #処理モード
  switch (${argv[1]})
    #バックアップ
    case "-b"
      set inFile=${FILE_PATH}
      breaksw
    #リストア
    case "-r"
      set inFile=${DISC_PATH}


#      set inFile="../tmp/kawamnt"


      breaksw
    #規定外(オプション指定エラー)
    default:
      set errcont="invalid exec. options"
      set errtag="xxxxx"
      set errf=1
      breaksw
  endsw
  if (${errf} == 0) then
    #ファイル指定
    switch (${argv[2]})
      #ファイル指定
      case "-f"
        set inFile="${inFile}/${argv[3]}"
        breaksw
      #ヒストリ指定
      case "-h"
        set inFile="${inFile}/*-${argv[3]}.sql"
        breaksw
      #規定外(オプション指定エラー)
      default:
        set errcont="invalid file select options"
        set errtag="xxxxx"
        set errf=1
        breaksw
    endsw
  endif
else
  #引数エラー
  set errcont="Not enough"
  set errtag="xxxxx"
  set errf=1
endif

#処理ディレクトリの確認
if (${errf} == 0) then
  set existDir=${FILE_PATH}
  if (! -e ${existDir}) then
    mkdir -m 777 ${existDir}
    if ($? != 0) then
      #処理ディレクトリ作成失敗
      set errcont="Temporary-directory make failed"
      set errtag="mkdir"
      set errf=1
    endif
  endif
  if (! -d ${existDir}) then
    #処理ディレクトリなし
      set errcont="can't find a Temporary-directory"
      set errtag="xxxxx"
      set errf=1
  endif
endif

#指定により各処理に分岐
if (${errf} == 0) then
  if ("${argv[1]}" == "-b") then
    #バックアップ処理
    #入力ファイルからisoファイルを作成
    genisoimage -quiet -r -J -o ${existDir}/discwrite.iso ${inFile}
    if ($? == 0) then
      set existFile="${existDir}/discwrite.iso"
      if (-e ${existFile}) then
#        echo "バックアップファイルをディスクに書き出します。"
#        echo "準備ができたら何かキーを押してください。"
        echo "I've done all of my preparing for backup-files to backup-disc,"
        echo "Please press enter key to continue..."
        set rd=$<
        #ディスクの消去
        sudo wodim dev=/dev/scd0 -blank=fast
        if ($? != 0) then
          #ディスク消去エラー
          set errcont="wodim exec.(erase) error"
          set errtag="wodim"
          set errf=1
        else
          #isoファイルをディスクに書き出し
          set cnt=0
          sudo wodim -nofix dev=/dev/scd0 -data "${FILE_PATH}/discwrite.iso"
          if ($? != 0) then
            #ディスク書き込みエラー
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
        #isoファイルなし
        set errcont="can't find a iso-files"
        set errtag="wodim"
        set errf=1
      endif
    else
      #iso作成エラー
      set errcont="genisoimage exec. error"
      set errtag="genisoimage"
      set errf=1
    endif
  else
    #リストア処理
#    echo "ディスクからバックアップファイルを取得します。"
#    echo "準備ができたら何かキーを押してください。"
    echo "I've done all of my preparing for get a backup-files from backup-disc,"
    echo "Please press enter key to continue..."
    set rd=$<
    #ディスクドライブをマウント
    sudo mount /dev/scd0 ${DISC_PATH}


#sudo mount -t iso9660 -o loop ../tmp/discwrite.iso "${FILE_PATH}/kawamnt"


    #指定ファイルをバックアップディレクトリに複写
    cp -f ${inFile} "${existDir}/."
    #ディスクドライブをアンマウント
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

#終了処理(実行判定)
if (${errf} == 0) then
  logger -t disc_bkup "disc_bkup OK"
  echo "disc_bkup OK"
  exit 0
else
  if (${?FN2} == 1) then
    echo "disc_bkup NG:${errcont}" | tee ${FN2} | logger -t "${errtag}" 
  else
    #FN2定義なし(コマンド実行時)
    echo "disc_bkup NG:${errcont}" 
  endif
  exit 1
endif
