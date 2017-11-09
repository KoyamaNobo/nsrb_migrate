<?php
//プロセスが実行中か調べる
//また終了しているときはtmpファイルのエラーメッセージを取得する
//  戻り値
//    引数が存在していないとき  ：ERR9018
//    pidが実行中のとき         ：true
//    pidが終了しているとき     ：ERR9015（ファイルが存在しないとき）
//                              ：ERR9016（添付ファイル削除に失敗したとき）
//                              ：メッセージ（添付ファイルの中身すべて）
require_once(dirname(__FILE__).'/lib/config.php');
require_once(dirname(__FILE__).'/lib/log.php');

//実行中か調べる
function pidCheck($pid,$filename){
	require_once(dirname(__FILE__).'/lib/config.php');
	require_once(dirname(__FILE__).'/lib/log.php');
	$oLog          = New Log(''); 
	$ret = "false";
	//プロセスIDは繰り返し使用されるため
	//インプットファイルとアウトプットファイルも確認コマンドに含める
	$filename = substr($filename, 0, -5); 
	
	$cmd = "ps -p " .$pid. " -o pid,stat,start,time,command | grep ".$filename." ";
	$oLog->info('file:'.__FILE__.' command:'.$cmd );
	$fp  = popen($cmd, "r");
	while( ($line = fgets($fp)) != false ){
		if( intval(trim($line)) == $pid ){
			$ret = "true";
			break;
		}
	}
	pclose($fp);
	return $ret;
}
//エラー内容を取得する
function getRespons($filename){
	$ret = "";
	require_once(dirname(__FILE__).'/lib/config.php');
	require_once(dirname(__FILE__).'/lib/log.php');
	$oLog = New Log(''); 
	//ファイルの存在チェック
	//結果がなくても作成されるので
	//ファイルが存在しない場合はエラー
	if(!file_exists($filename)){
		$ret = 'ERR9015';
		return $ret;
	}
	//ファイル読み込み
	$respons = file_get_contents($filename);
	//ログ出力
	$oLog->info('file:'.__FILE__.' INFO:'.$respons );
	//ファイル削除
	if(!unlink($filename)){
		$ret = 'ERR9016';
		return $ret;
	}
	//結果出力
	return $respons;
}

$ret = "false";
$oLog          = New Log(''); 

//POSTの引数が存在しているか
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['word']) && !empty($_POST['word']) ){
		//POSTされた値をpidとtmpファイル名に分割
		$data=explode(",",$_POST['word']);
		$pid = $data[0];
		$tmpfile = $data[1];
		$oLog->info('file:'.__FILE__.'pid:'.$pid.' outfile'.$tmpfile );
		//プロセスIDが実行中か調べる
		$ret = pidCheck($pid,$tmpfile);
		if($ret == "false"){
			//エラーを取得する
			$ret = getRespons($tmpfile);
		}
	}else{
        echo('ERR9018');
        exit();
	}
}

echo $ret;
?>
