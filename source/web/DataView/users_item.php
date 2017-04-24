<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');
//専用ライブラリ読み込み
require_once('./lib/clsDBM_Extension.php');

//変数初期化
$post          = $_POST;
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

$clsData = New clsDBM_Extension($dbhDV,$_SESSION['dv_user_id'],DB_NAME,$_SESSION['dv_user_disp_num']);

//インサートorアップデート処理
if(!empty($post)){
	//受け取ったパラメータをモジュールに渡す
	$clsData->setState($post);

	//postで受け取った情報を元にした行数を取得
	$loopCount = $clsData->getRowCountWithState();
	if(array_key_exists('action',$post) && preg_match('/^del.*/',$post['action']) == 1){
		//存在しているときはアップデート
		$success = 0;
		$success = $clsData->delData();
		if($success){
			//削除成功時
			$successCode   = 'INF0007';
		}else{
			//削除失敗時
			$message_codes = 'ERR0020';
			put_error($message_codes,'');
		}
	}else if($loopCount != 0){
		//存在しているときはアップデート
		$success = 0;
		$success = $clsData->updateData();
		if($success){
			//インサート成功時
			$successCode   = 'INF0008';
		}else{
			//インサート失敗時
			$message_codes = 'ERR0017';
			put_error($message_codes,'');
		}

	}else{
		//存在していないときはインサート
		$success = 0;
		//インサートし成功したかどうか
		$success = $clsData->insertData();

		if($success){
			//インサート成功時
			$successCode   = 'INF0008';
		}else{
			//インサート失敗時
			$message_codes = 'ERR0018';
			put_error($message_codes,'');
		}

	}
}
//項目一覧取得
//upd koyama 後で追加をAjaxで取得する
$clsData->get_items(0);

//画面描画d
require_once('./view/vw_users_item.php');

?>
