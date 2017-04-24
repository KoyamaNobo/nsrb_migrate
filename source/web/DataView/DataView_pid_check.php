<?php

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//POSTの引数が存在しているか
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['PID']) && !empty($_POST['PID']) ){
		$post        = $_POST;
		$pid         = $post['PID'];
		$outputfile  = CSV_PATH .$post['Output_file']; //レコード件数
	}else{
	    echo('ERR0501');
        put_error('ERR0501','');
        exit();
	}
}

$ret = "false";
//プロセスIDは繰り返し使用されるため
//インプットファイルとアウトプットファイルも確認条件タグに含める
$cmd = "/bin/ps -p " .$pid. " -o pid,stat,start,time,command ";


$fp  = popen($cmd, "r");
while( ($line = fgets($fp)) != false ){
	if( intval(trim($line)) == $pid ){
		$ret = "true";
		break;
	}
}

$ret = "PIDinfo:".$ret;

//書き込み完了件数を調べる
$count_str = exec( 'wc -l '.$outputfile );
$count = explode(" ", $count_str);

$ret .= ",WritingCount:".$count[0];


//PIDの状態と書き込み完了件数を出力する
echo $ret;

?>