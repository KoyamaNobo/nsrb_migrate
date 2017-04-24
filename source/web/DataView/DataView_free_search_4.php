<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//抽出条件分解関数
function sfSprit(&$selectedFilters,$postFilters){
	foreach($postFilters as $tmp){
		$tmps = explode("~", $tmp);
		$add = array();
		$add["andOr"]        = $tmps[0];
		$add["tableName"]    = $tmps[1];
		$add["itemName"]     = $tmps[2];
		if($tmps[3]==""){
			$add["JapaneseName"] = " "; //空だっただスペースを代入することで表示時に「&nbsp;」に変換される
		}else{
			$add["JapaneseName"] = $tmps[3];
		}
		$add["s_point"]      = $tmps[4];
		$add["size"]         = $tmps[5];
		$add["type"]         = $tmps[6];
		$add["operator"]     = $tmps[7];
		$add["value"]        = $tmps[8];
		$add["id"]           = $tmps[9];
		array_push($selectedFilters, $add);
	}
}

//変数初期化
$post              = $_POST;
$selectedBonds     = array();
$selectedItemNames = array();
$selectedFilters   = array();
$selectedTables    = array();
$sf                = array();
$message_codes     = '';


//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}


if(!empty($post)){
	//選択テーブル
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}

	//結合条件
	foreach($post["selectedBonds"] as $tmp){
		array_push($selectedBonds, $tmp);
	}

	//表示項目
	foreach($post["selectedItemNames"] as $tmp){
		array_push($selectedItemNames, $tmp);
	}

	//抽出条件(戻る画面から戻ってきた場合）
	if(!empty($post["selectedFilters"])){
		sfSprit($selectedFilters,$post["selectedFilters"]);
	}
	//抽出条件（追加画面から遷移した場合）
	if(!empty($post["selectedFilter"])){
		sfSprit($selectedFilters,$post["selectedFilter"]);
	}
}else{
	//URLのみで呼び出された時
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//画面描画
require_once('./view/vw_DataView_Free_Search_4.php');

?>
