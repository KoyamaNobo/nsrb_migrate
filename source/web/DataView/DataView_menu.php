<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$message_codes = '';                         //エラーメッセージコードを格納

//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//画面描画
require_once('./view/vw_DataView_Menu.php');

?>
