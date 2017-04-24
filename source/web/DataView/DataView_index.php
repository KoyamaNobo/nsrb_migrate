<?php
//セッションスタート
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post         = $_POST;                                //post値を格納
$get          = $_GET;                                 //get値を格納
$loginFlg     = false;                                 //メニュー画面に遷移してもよいかのフラグを格納
$message_codes = "";                                   //エラーメッセージコードを格納

//post値取得
if(!empty($post['dv_user_id'])){
	//ログインボタン押下時
	//DB接続（DataView用コネクション:$dbhDVの作成）
	if(db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
		//DB接続に成功した場合
		//ログインチェック
		if(loginCheck($post['dv_user_id'], $post['dv_password'],$message_codes,$dbhDV)){
			//ログインチェックに成功した場合
			$loginFlg = true;
		}
	}
}else if(!empty($get['message'])){
	//リダイレクトで戻ってきた場合（不正処理）
	$message_codes = $get['message'];
}

//画面描画
if($loginFlg){
	require_once('./view/vw_DataView_Menu.php');
}else{
	require_once('./view/vw_DataView_Login.php');
}

?>
