<?php
////////////////////////////////////////////////////////////////////////////////
//共通ライブラリ読み込み
////////////////////////////////////////////////////////////////////////////////
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//実行時間無限大
////////////////////////////////////////////////////////////////////////////////
set_time_limit(0);

////////////////////////////////////////////////////////////////////////////////
//変数初期化
////////////////////////////////////////////////////////////////////////////////
$sql         = '';
$sqlExec     = '';
if(isset($argv[1])){
	$sql         = $argv[1];
	//「シングルクォートが「##」で渡ってくるので逆変換
	$sql = str_replace("##", "'", $sql);
}
$sqlexe       = '';
$limit_start  = 0;
$contents     = '';
$disps        = array();
$breakFlag    = false;
$message_codes = '';

////////////////////////////////////////////////////////////////////////////////
//DB接続（日進用コネクション:$dbhNISの作成）
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB接続に失敗した場合
	echo $message_codes."\r\n";
	return false;
}

//SQL文からLIMIT文を外したものを取得（文末の「;」も外したもの）
$sqlExec = getSqlExec($sql);

//実行SQLより画面表示用の項目名の部分を取得
$disps = getSqlDisps($sqlExec);


while ($breakFlag == false ) {
	//SQL文作成
	$sqlexe  = $sqlExec; //毎回初期化（LIMITを変えるため）
	$sqlexe .= ' LIMIT '.$limit_start.' , '.CSVNUM.' ; ';
	$sth = $dbhNIS->prepare($sqlexe);
	//SQL実行
	$sth->execute();
	
	$dataCount = 0;
	while($data = $sth->fetch(PDO::FETCH_ASSOC)){
		$loopCount = 0;
		$contents = '';
		foreach($disps as $disp){
			if($loopCount != 0 ){
				$contents .= ',';
			}
			$contents .= $data[$disp["dispname"]];
			$loopCount++;
		}
		
		//CSVに書き込み
		echo $contents . PHP_EOL;
		$dataCount++;
	}
	
	if($dataCount == 0){
		//データが存在しないとき処理を抜ける
		$breakFlag = true;
	}
	
	//次のSQL発行用にLIMIT値を変える
    $limit_start = $limit_start + CSVNUM;
}

?>