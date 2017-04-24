<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');
//専用ライブラリ読み込み
require_once('./lib/clsDBM_Extension.php');

//変数初期化
$post          = $_REQUEST;
$TableName     = '';
$ItemName      = '';
$Japanese_Name = '';
$NonDisp_Flg   = '';
$S_point       = '';
$Size          = '';
$Disp_Num      = '';
$today         = date("Y-m-d H:i:s");
$Cre_Date      = $today;
$Mod_Date      = $today;
$Del_Flg       = '0';
$items         = array();
$successCode   = '';
$message_codes = '';

//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//DB接続（日進用コネクション:$dbhNISの作成）
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB接続に失敗した場合
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//専用ライブラリのクラスインスタンス
$clsData = New clsDBM_Extension($dbhDV,$_SESSION['dv_user_id'],DB_NAME,$_SESSION['dv_user_disp_num']);

//項目一覧取得
//upd koyama 後で追加をAjaxで取得する
if(!empty($post) && array_key_exists('num',$post)){
	$clsData->get_items(($post['num']));
}else{
	$clsData->get_items(0);
}
shell_exec("logger -i 'testtest".print_r($clsData->tableData,true)."test '");
//画面描画d
require_once('./view/vw_user_item_listadd_ajax.php');

?>
