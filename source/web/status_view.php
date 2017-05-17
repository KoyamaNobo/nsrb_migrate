<?php
//
//status表示用コントローラ
//2017/04/14 create koyama
require_once('./lib/config.php');
require_once('./lib/log.php');
session_start();
define('STATUS_MODE_PROC','mproc');
define('STATUS_MODE_DB','mdb');

#TODO:add koyama CHARSETはどうする 現状UTF-8 20170714
$oConf         = New initConf();                    //config.php内コンフィグ用クラス
$oLog          = New Log('');                       //ロギングクラス
$oStatus;                                           //後続の処理で何を入れるかを変える
$status_mode   = '';                                //モード:現在とは逆のものが入る
$page_num      = 1;

$sani = $_GET;
$message = "";

if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
    //ログイン失敗時
    $oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
    header('Location: ./sessDestroy.php?mess='.$message.'');
    exit;
}

//空で無ければ値を取得
if(!empty($sani['status-mode'])){
    $status_mode = $sani['status-mode'];
}

#プロセスの状態チェック
if(select_mode($status_mode) != STATUS_MODE_DB){
    require_once('./lib/clsProcessStat.php');
    $oStatus = New clsProcessStat();
    //切り替えるとすると次は逆になる
    $status_mode = STATUS_MODE_PROC;
}else{
    require_once('./lib/clsDBStat.php');
    $oStatus = New clsDBStat();
    //切り替えるとすると次は逆になる
    $status_mode = STATUS_MODE_DB
}
require_once('./view/vw_status.php');

//この処理に必要な関数を別で着る
function select_mode($str_mode){
    if($str_mode == STATUS_MODE_DB){
        return STATUS_MODE_DB;
    }
    return STATUS_MODE_PROC;

}
?>
