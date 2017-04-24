<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//実行時間無限大
////////////////////////////////////////////////////////////////////////////////
//set_time_limit(0);

//POSTの引数が存在しているか
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['Output_file']) && !empty($_POST['Output_file']) ){
		$post        = $_POST;
		$outputfile  = $post['Output_file']; //ダウンロードのファイル名
	}
}else{
		put_error('ERR0501','');
		exit();
}

//ダウンロード時
ini_set('memory_limit', '3000M');
header('Content-Disposition: attachment; filename="' . $outputfile . '"');
header('Content-Type: application/octet-stream');
header('Content-Transfer-Encoding: binary');
header('Content-Length: ' . filesize(CSV_PATH . $outputfile));

if(!readfile(CSV_PATH . $outputfile)){
    put_error('ERR0019','');
}

?>