<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$tables            = array();                               //DBよりテーブル名の一覧取得の結果を入れる
$post              = $_POST;                                //post値を格納（戻るボタンで遷移してきたとき）
$selectedTables    = array();                               //選択テーブルを格納
$message_codes     = '';                                    //メッセージを格納

//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//DB接続（日進用コネクション:$dbhNISの作成）
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//POST値取得
if(!empty($post)){
	//戻るで遷移したとき
	//選択テーブル取得
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}
}

//DBよりテーブル名の一覧取得
get_all_tables($tables,$message_codes,$dbhNIS);

//画面描画
require_once('./view/vw_DataView_Free_Search_1.php');

?>
