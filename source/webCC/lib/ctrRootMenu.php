<?php
//cmdが無いときは中身をすべて無視
//if(!empty($sani['root_cmd'])){
	//何か問題が起きた時にデフォルトのメニュー
	$sani['root'] = $_SESSION['def_name'];
	//cmdが送られてくるときはSESSIONから取る
	if(!empty( $_SESSION['user_name']) || !empty($_SESSION['password']) ){
		//すでにログインしてあれば再ログインしない仕様
		$user = $_SESSION['user_name'];
		$pass = $_SESSION['password']; 
	}else{
		$user = '';
		$pass = '';
	}
	if(!$oConf->loginCheck($user, $pass ,$message )){
		//失敗
		if(isset($sani['mess'])){
			$message = $sani['mess'];
		}
		require_once('./rtView/vw_index.php');
		exit;
	}
	require_once('./lib/clsExecScreen.php');
	$termFlg = true;
	
	//画面に表示させるため、画面をオブジェクトを流用
	require_once('./rtView/vw_roexec.php');
	exit;
	$oLog->info('file:'.__FILE__.':'.__LINE__.' targfile:'.SOURCEDIR.$strMenuDir.$sani['root'].'.sh');
//}
?>