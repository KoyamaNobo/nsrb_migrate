<?php
//
//gmenu用テスト画面
//20170203 create koyama
session_start();
require_once('./lib/config.php');
require_once('./lib/clsMenuItem.php');
require_once('./lib/log.php');

$fileName      = DEFFNAME;
$strType       = DEFKIND;                          //処理のタイプを格納
$oConf         = New initConf();                    //config.php内コンフィグ用クラス
$oLog          = New Log('');                      //ロギングクラス
$oProcs;
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
	$userId = $_SESSION['user_id'];
	require_once('./lib/clsProcInfo.php');
	$oProcs = New clsProcInfo('run',$userId);

	$arrResult['procNum'] = $oProcs->getCountProcArray();
	//実行中のメニュー名を表示
	$arrResult['procList'] = $oProcs->getHTMLToProcs();
	echo $oProcs->getHTMLToProcs();
	exit;
?>
