<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post              = $_POST;
$selectedBonds     = array();
$selectedItemNames = array();
$selectedFilters   = array();
$selectedSorts     = array();
$selectedTables    = array();
$message_codes     = '';
$listTypeFlg       = false; //vw_Part_DataView_ItemNamesで使用

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

	//結合条件
	foreach($post["selectedBonds"] as $tmp){
		array_push($selectedBonds, $tmp);
	}

	//表示項目
	foreach($post["selectedItemNames"] as $tmp){
		array_push($selectedItemNames, $tmp);
	}

	//抽出条件
	if(isset($post["selectedFilters"])){
		foreach($post["selectedFilters"] as $tmp){
			array_push($selectedFilters, $tmp);
		}
	}

	//並べ替え設定
	if(isset($post["selectedSorts"])){
		foreach($post["selectedSorts"] as $tmp){
			$tmps = explode("~", $tmp);
			$add = array();

			//AHNHF~AHNH-KEY~~asc~1~7~~AHNHF

			$add["listTableName"]      = $tmps[0];
			$add["listItemName"]       = $tmps[1];
			$add["listJapansesName"]   = $tmps[2];
			$add["listId"]         = $tmps[7] . "." . $tmps[1];
			if($tmps[3] == "asc"){
				$add["listSort"]       = "昇順";
			}else{
				$add["listSort"]       = "降順";
			}
			$add["listS_point"]    = $tmps[4];
			$add["listSize"]       = $tmps[5];
			$add["listType"]       = $tmps[6];
			$add["id"]             = $tmps[7].".".$tmps[1];
			$add["listNext"]       = $tmp;
			array_push($selectedSorts, $add);
		}
	}
}else{
	//URLのみで呼び出された時
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}



//画面描画
require_once('./view/vw_DataView_Free_Search_6.php');

?>
