<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post               = $_POST;
$selectedBonds      = array();
$selectedItemNames  = array();
$selectedTables     = array();
$message_codes       = '';

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
	//選択テーブル取得
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}
	//結合条件取得
	foreach($post["selectedBonds"] as $tmp){
		array_push($selectedBonds, $tmp);
	}
	//表示項目取得（データ変換）
	if(isset($post['selectedItemNames'])){
		foreach($post["selectedItemNames"] as $tmp){
			$tmps = explode("~", $tmp);
			$add = array();
			$add["listTableName"]      = $tmps[0];
			$add["listItemName"]       = $tmps[1];
			if($tmps[2] == ""){
				$add["listJapaneseName"]   = " "; //空だったらスペースを入れることで「&nbsp;」に変換される
			}else{
				$add["listJapaneseName"]   = $tmps[2];
			}
			$add["listId"]             = $tmps[3];
			$add["listS_point"]        = $tmps[4];
			$add["listSize"]           = $tmps[5];
			$add["listType"]           = $tmps[6];
			$add["id"]                 = $tmps[3];
			array_push($selectedItemNames, $add);
		}
	}
}else{
	//URLのみで呼び出された時
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//画面描画
require_once('./view/vw_DataView_Free_Search_3.php');

?>
