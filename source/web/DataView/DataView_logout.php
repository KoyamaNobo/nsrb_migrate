<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$message_codes = "";


//セッションを破壊する
//クッキー情報ももしあれば以下の例の記述で削除すること
//  setcookie("クッキーの変数名", $count, time() - 1800);
//session_destroy();
//他のセッションを壊さないために
$_SESSION['dv_user_id']            = '';
$_SESSION['dv_user_password']      = '';
$_SESSION['dv_user_authority_flg'] = '';
$_SESSION['dv_user_disp_num']      = '';
//画面描画
require_once('./view/vw_DataView_Login.php');

?>
