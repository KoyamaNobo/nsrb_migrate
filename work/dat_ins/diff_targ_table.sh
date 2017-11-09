#!/bin/bash
################set -e エラーが出るとそこで終了してくれる？
#set -x
CURRPTH=`pwd`
NEWHOST="localhost"
OLDHOST="localhost"
NEWDB=""
NEWUSER=""
NEWPASS=""
OLDDB=""
OLDUSER=""
OLDPASS=""
result=0

if [ $# -eq 4 ];then
    NEWDB=$2
    NEWUSER=$3
    NEWPASS=$4
elif [ $# -eq 7 ];then
    NEWDB=$2
    NEWUSER=$3
    NEWPASS=$4
    OLDDB=$5
    OLDUSER=$6
    OLDPASS=$7
else
    echo "引数は4個または7個指定する必要があります。"
    exit
fi

#引数が4か7だった場合は取り込み処理を実行

if [ -e ${1} ];then
    echo "data insert start ${1}" |logger -i
    #指定ファイルのパスに移動する
    TARGDIR=`dirname "${1}"`
    FILENAME=`basename "${1}"`
    cd $TARGDIR
    #ディレクトリを探してシンボリックリンクを貼っておく
    DATEDIR=''
    DIRNAMES=`ls -alt | tail -n +2 |awk '$(NF)~/^[0-9]+$/ {print $(NF)}'`
    ls -alt | tail -n +2 |awk '$(NF)~/^[0-9]+$/ {print $(NF)}' > temp.txt
    #日付の新しいディレクトリを探す
    while read LINE ;do
      if [[ $DATEDIR < $LINE ]];then
        DATEDIR="$LINE"
      fi
    done < <(ls -alt | tail -n +2 |awk '$(NF)~/^[0-9]+$/ {print $(NF)}')
    echo "ln -s $DATEDIR datadir"
    ln -s $DATEDIR datadir
    "./$FILENAME"  2>&1 |tee ${FILENAME}.log
    rm datadir
    echo "data insert end ${1}" |logger -i
else
    echo "file dase not exists!!"
    exit
fi

#引数が7の場合のみDB間比較処理を実行
if [ $# -eq 7 ];then
    QUERY="SHOW TABLE STATUS FROM $NEWDB WHERE NOT comment LIKE '%VIEW%' AND NOT Name LIKE '%\_%' AND NOT Name LIKE 'WK%' AND NOT Name LIKE 'JT-W%' AND NOT Name REGEXP '^W[0-9]' ;"
    RET=(`mysql --host=$NEWHOST --user=$NEWUSER --password=$NEWPASS -N -e"$QUERY" | awk '{print $1}'`)
    #echo ${#RET[@]}
    for i in `seq 1 ${#RET[@]}`
    do
	#echo ${RET[$i-1]}
	tname=${RET[$i-1]}
	QUERY="select count(*) from ( select * from $NEWDB.\`$tname\` union select * from $OLDDB.\`$tname\`)  as uni group by ID HAVING COUNT(*) = 2"
	RET=`mysql --host=$NEWHOST --user=$NEWUSER --password=$NEWPASS -e"$QUERY" |wc -l`
	if [ $((RET)) -gt 0 ] ; then
		#テーブル名,DB間データ不一致であることを出力
		echo $tname":データ不一致" |logger -i
	fi
    done
fi

#移動していない可能性があっても元の場所に戻る
cd $CURRPATH
