<?php
//
//gmenu用テスト画面
//20170515 create koyama
session_start();
require_once('./lib/config.php');
require_once('./lib/clsMenuItem.php');
require_once('./lib/log.php');

define('STATUS_MODE_PROC','mproc');
define('STATUS_MODE_DB','mdb');

$fileName      = DEFFNAME;
$strType       = DEFKIND;                          //処理のタイプを格納
$oConf         = New initConf();                    //config.php内コンフィグ用クラス
$oLog          = New Log('');                      //ロギングクラス
$oStatus;                                          //後続の処理で何を入れるかを変える
$temp          = '';
$arrResult     = array();

$sani = $_GET;
$message = "";
if(!empty($sani['mess'])){
	$message = $sani['mess'];
}

//*************************************************************************入力によるTYPEの切り分け
//種別がLMだったときから移植してStart
//ログインできていなければ初期画面に戻す
if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
    //ログイン失敗時
    $oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
    header('Location: ./sessDestroy.php?mess='.$message.'');
    exit;
}
$page_num      = 1;
$sani          = $_REQUEST;
$message       = "";
$buttonName   = "";

//空で無ければ値を取得
if(!empty($sani['status_mode'])){
    $status_mode = $sani['status_mode'];
}
if(!empty($sani['page_num'])){
	if((int)$sani['page_num'] < 1){
		$page_num = 1;
	}else{
		$page_num = (int)$sani['page_num'];
	}
}

#プロセスの状態チェック
if(select_mode($status_mode) != STATUS_MODE_DB){
    require_once('./lib/clsProcessStat.php');
    $oStatus = New clsProcessStat();
    //切り替えるとすると次は逆になる
    $status_mode  = STATUS_MODE_DB;
    $current_mode = STATUS_MODE_PROC;
	$buttonName   = "DB";
}else{
    require_once('./lib/clsDBStat.php');
    $oStatus = New clsDBStat();
    //切り替えるとすると次は逆になる
    $status_mode  = STATUS_MODE_PROC;
    $current_mode = STATUS_MODE_DB;
	$buttonName   = "PG";
}

echo $oStatus->getLineWithPageNum($page_num);
echo '<input id="send_count" name="send_count" type="hidden" value="';
echo $oStatus->countStatusLine();
echo '" />';
echo '<input id="send_max" name="send_max" type="hidden" value="';
echo $oStatus->countStatusMax();
echo '" />';
exit;

//この処理に必要な関数を別で着る
function select_mode($str_mode){
    if($str_mode == STATUS_MODE_DB){
        return STATUS_MODE_DB;
    }
    return STATUS_MODE_PROC;

}
?>
