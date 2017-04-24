<?php
require_once(dirname(__FILE__).'/lib/config.php');
require_once(dirname(__FILE__).'/lib/log.php');

//ランダムの文字列作成
function makeRandStr($length) {
	$str = array_merge(range('a', 'z'), range('0', '9'), range('A', 'Z'));
	$r_str = null;
	for ($i = 0; $i < $length; $i++) {
		$r_str .= $str[rand(0, count($str))];
	}
	return $r_str;
}

$strInp = $_POST['word'];
$err_flg = "0";
$check_flg = "0";
$file_name = "";
$strConditions = "";
$oLog  = New Log(''); 

//POSTパラメータの存在確認
if(!isset($strInp) || empty($strInp) ){
// 	shell_exec("logger -i 'test ^-^ test ".print_r($_POST,true)."'");
	echo('ERR9001');
	exit();
}

//「’」で囲まれた部分（項目名）を全て取り除く
$strSend = "";
$strInpNonDispName = "";
$count = 0;
$tmp = explode("'", $strInp);
$dispName = array();
foreach ($tmp as $value) {
	if($count % 2 == 0){
		//パラメータのみを抽出
		$strSend = $strSend . $value;
	}
	else{
		//項目名のみを配列へ格納
		array_push($dispName,$value);
	}
	$count = $count + 1;
}

//表示項目と検索条件に分割
$tmp = explode("__", $strSend);
$strDispItems = $tmp[0];
if( isset($tmp[1]) || empty($strInp)){
	$strConditions = $tmp[1];
}

//表示項目を各パラメータに分割
$strDispItemsArry = explode(" ", $strDispItems);

foreach ($strDispItemsArry as $value) {
	$res = preg_match("/PB1=/", $value); 
	if($res){
		// パラメータの中にPB1が存在しているとき
		$tmp = str_replace("PB1=", "", $value);
		$csv_file_name = $tmp;
		$txt_file_name = str_replace(".CSV", "", $tmp);
		$txt_file_name = $txt_file_name . ".TXT";
	}
}

//PA5の中身をFNCNV用にパラメータ変換
$strPA5 = "PA5=";
$count = 0;
foreach ($strDispItemsArry as $value) {
	$res = ereg("PA5=", $value); 
	if($res){
		// パラメータの中にPB1が存在しているとき
		$tmp = str_replace("PA5=", "", $value);
		$tmpArry = explode("^", $tmp);
		foreach ($tmpArry as $dispItem) {
			$dispItemArry = explode(",", $dispItem);
			if ($count != 0){
				$strPA5 = $strPA5 . ",";
			}
			if(count($dispItemArry) == 5){
				$strPA5 = $strPA5 . substr($dispItemArry[3], 0, 1) . (string)((int)$dispItemArry[1] . substr($dispItemArry[3], 1, 1). (int)$dispItemArry[2]);
			}
			else{
				$strPA5 = $strPA5 . substr($dispItemArry[2], 0, 1) . $dispItemArry[1];
			}
			$count = $count + 1;
		}
	}
}

//パラメータ変換したものを再結合してFNCNV用にパラメータ作成
$strFNCNVsend = "";
$count = 0;
$tmpArry = explode(" ", $strSend);
foreach ($tmpArry as $value) {
	if ($count == 0){
		//PA5はFNCNV用にパラメータ変換したものに置き換える
		$strFNCNVsend = $strFNCNVsend . $strPA5;
	}else{
		//SORTの仕様変更（データ型「P,Q」は桁数ではなくバイトで指定）にて追加
		$res = ereg("SEL=", $value); 
		if($res){
			$selcount = 0;
			$tmpSelArry = explode(",", $value);
			foreach ($tmpSelArry as $selvalue) {
				$selcount++;
				$m = $selcount % 4;
				if($m == 3 && ($selvalue == "P" || $selvalue == "Q")){
					//余りが３のパラメータ（型のパラメータ）が「Q,P」の時
					//一つ前のパラメータを桁数からバイト数へ変換
					$tmpSelArry[$selcount-2] = ceil((intval($tmpSelArry[$selcount-2])/2)+0.5);
				}
			}
			$selcount = 0;
			foreach ($tmpSelArry as $selvalue) {
				$selcount++;
				if($selcount == 1){
					$strFNCNVsend = $strFNCNVsend . " " .  $selvalue;
				}else{
					$strFNCNVsend = $strFNCNVsend . "," .  $selvalue;
				}
			}
		}else{
			//そのまま結合
			$strFNCNVsend = $strFNCNVsend ." " .  $value;
		}
	}
	$count = $count + 1;
}

//TXTの書き込む内容を再組み立て
//※SELに関してのパラメータはPHPへ渡された引数（桁数）のまま作成
$strTxt = "PA5=";
$loop_count = 0;
foreach ($strDispItemsArry as $value) {
	$res = ereg("PA5=", $value); 
	if($res){
		// パラメータの中にPB1が存在しているとき
		$tmp = str_replace("PA5=", "", $value);
		$tmpArry = explode("^", $tmp);
		foreach ($tmpArry as $dispItem) {
			$dispItemArry = explode(",", $dispItem);
			if($loop_count != 0)
			{
				$strTxt = $strTxt . "^";
			}
			if(count($dispItemArry) == 5){
				$strTxt = $strTxt . $dispItemArry[0]  . ",". $dispItemArry[1]  . ",". $dispItemArry[2]  . ",". $dispItemArry[3] . ",'" . $dispName[$loop_count] . "'" . $dispItemArry[4]; 
			}else{
				$strTxt = $strTxt . $dispItemArry[0]  . ",". $dispItemArry[1]  . ",". $dispItemArry[2] . ",'" . $dispName[$loop_count] . "'" . $dispItemArry[3]; 
			}
			$loop_count= $loop_count+ 1;
		}
	}
}

if(!is_null($strConditions) && !empty($strConditions))
{
	//FLCNV（テキスト出力用）用のパラメータ作成
	$strTxt = $strTxt . "__" . $strConditions;
}

// txtファイルが存在していなければファイル作成
if( !file_exists($txt_file_name) ){
	if(!touch($txt_file_name)){
		echo('ERR9003');
		exit();
	}
	chmod($txt_file_name,0777);
}

// ファイルが存在しかつ書き込み可能かどうか確認します
if (is_writable($txt_file_name)) {
	// この例では$txt_file_nameを追加モードでオープンします。
	// ファイルポインタはファイルの終端になりますので
	// そこがfwrite()で$strSendが追加される位置になります。
	if (!$handle = fopen($txt_file_name, 'w')) {
		 echo('ERR9004');
		 exit;
	}
	// オープンしたファイルに$strSendを書き込みます
	if (fwrite($handle, $strTxt) === FALSE) {
		echo('ERR9005');
		exit;
	}
	fclose($handle);
} else {
	echo('ERR9006');
	exit;
}

//バックグラウンドで起動させるプログラム（DataUtil2CSV.php）の出力先決定
$outputfile = $csv_file_name . makeRandStr(5);

//出力先決定が既に存在しているかチェック
//存在している場合は出力先を変える
while(file_exists($outputfile)){
	$outputfile = "../tmp/" . $file_name . makeRandStr(5);
}

//出力先ファイル作成
if(!touch( $outputfile)){
	echo('ERR9011');
	exit();
}
chmod($outputfile,0777);

//出力される前に作っておく？
if(!file_exists($outputfile)){
	touch($outputfile);
	chmod($outputfile,0777);
}

//バックグラウンドでジョブ実行
$output = array();
$cmd = "php ./DataUtil2CSV.php '".$strFNCNVsend."' >".$outputfile." 2>&1 & echo $! ";
$oLog->info('file:'.__FILE__.' command:'. $cmd );
exec($cmd,$output);

//戻り値としてプロセスIDとアウトプットファイル名を返す
echo "PID:".$output[0];
echo "OutPutFile:".substr($outputfile,6);

?>