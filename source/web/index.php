<?php
//
//gmenu用テスト画面
//20131125 create koyama
require_once('./lib/config.php');
require_once('./lib/clsMenuItem.php');
require_once('./lib/log.php');
session_start();

$fileName      = DEFFNAME;
$strType       = DEFKIND;                          //処理のタイプを格納
$oConf         = New initConf();                    //config.php内コンフィグ用クラス
$oLog          = New Log('');                      //ロギングクラス
$oProcs;
$temp          = '';
$arrSelectItem = array();
$strMenuDir  = 'menu/';
$arrJclDir  = 'job/';
$arrLmDir  = 'exec/';
$arrResult     = array();
$jobName = '';

$sani = $_GET;
$message = "";
if(!empty($sani['mess'])){
	$message = $sani['mess'];
}

//*************************************************************************入力によるTYPEの切り分け
//textboxに何か入力した場合?
if(!empty($sani['root'])){
	//ファイルの存在が確認できたら参照先を入れ替え
	//$arrMenuDir[0]はメニューのフォルダである想定　仕様変更時は用確認
// 	$oLog->info('file:'.__FILE__.' targfile:'.SOURCEDIR.$strMenuDir.$sani['root'].'.sh');
	if(file_exists(SOURCEDIR.$strMenuDir.strtolower($sani['root']).'.sh')){
		$fileName = SOURCEDIR.$strMenuDir.strtolower($sani['root']).'.sh';
		$strType = 'PM';
	}
}

//$fileName = SOURCEDIR.$fileName;
//リンクから来た場合?
if(!empty($sani['filename']) && empty($sani['root'])){
	if(isset($sani['typ'])){
		//MENU
		switch ($sani['typ']) {
			case strtolower($oConf->getKind(0)):
				//ファイルの存在が確認できたら参照先を入れ替え////////PM
				if(file_exists(SOURCEDIR.$strMenuDir.$sani['filename'].'.sh')){
					$fileName = SOURCEDIR.$strMenuDir.$sani['filename'].'.sh';
					$strType = $oConf->getKind(0);
				}
				break;
			case strtolower($oConf->getKind(1)):
				//ファイルの存在が確認できたら参照先を入れ替え//////JS
				if(file_exists(SOURCEDIR.$arrJclDir.$sani['filename'].'.sh')){
					$fileName = SOURCEDIR.$arrJclDir.$sani['filename'].'.sh';
					$strType = $oConf->getKind(1);
				}
				break;
			case strtolower($oConf->getKind(2)):
				//ファイルの存在が確認できたら参照先を入れ替え        //LM
				$sani['filename'] = strtoupper($sani['filename']);
				if(file_exists(SOURCEDIR.$arrLmDir.$sani['filename'].'')){
					$fileName = SOURCEDIR.$arrLmDir.$sani['filename'].'';
					$strType = $oConf->getKind(2);
				}
				break;
			case strtolower($oConf->getKind(3)):
				//ファイルの存在が確認できたら参照先を入れ替え        //LM
				$sani['filename'] = strtoupper($sani['filename']);
				if(file_exists(SOURCEDIR.$strMenuDir.mb_strtolower($sani['filename']).'')){
					$fileName = SOURCEDIR.$strMenuDir.mb_strtolower($sani['filename']).'';
					$strType = $oConf->getKind(3);
				}
				break;

			default:
				# code...
				break;
		}
	}
}

//*************************************************************************procInfo用の設定
if(array_key_exists('procInfo',$sani)){
//種別がLMだったとき
//ログインできていなければ初期画面に戻す
	$oLog->info('[procInfo execute]'.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password'].__FILE__.':'.__LINE__);
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//ログイン失敗時
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId = $_SESSION['user_id'];
	$title = '';
	$pid = 0;
	require_once('./lib/clsScreen.php');
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/clsProcInfo.php');
	$title = getExecTitle($sani['filename']);

	$oProcs = New clsProcInfo('run',$userId);
	$clsBP = New AsynchronousProcess($sani['outfname'],$sani['infname']);
	$screen = New clsScreen();
	//プロセスの状態取得
	//TODO:ここでpidが取れないと処理がおかしくなる koyama 20161018
	//upd koyama 20161018 Pidが取れない
	$pid = $clsBP->setPid();
	if($pid <= 0){
		if($_SERVER["HTTP_REFERER"] != ''){
			$oLog->error('[not found process_id]'.__FILE__.':'.__LINE__.' uri:'.$_SERVER["REQUEST_URI"].' ref:'.$_SERVER["HTTP_REFERER"]);
			header('Location: ./index.php?root='.$_SESSION['def_name'].'');
		}
	}
	$statArray = getProcessIdStatus($clsBP->setPid(),$oLog);
	require_once('./view/vw_exec.php');
	exit;
}
//*************************************************************************TYPEによる表示の切り分け
// var_dump($strType);
// $oLog->info('file:'.__FILE__.':'.__LINE__.' var:'.print_r($sani,true).$strType);
if($strType === DEFKIND){
	//種別が初期値(空文字列)のとき
	//POSTパラメータが送られてこないときはSESSIONの値でログインをトライ
	if(!empty($_POST['account']) || !empty($_POST['passwd']) ){
		$user = $_POST['account'];
		$pass = $_POST['passwd'];
	} elseif(!empty( $_SESSION['user_name']) || !empty($_SESSION['password']) ){
		//すでにログインしてあれば再ログインしない仕様
		$user = $_SESSION['user_name'];
		$pass = $_SESSION['password'];
	}else{
		$user = '';
		$pass = '';
	}
	if(!empty($user) && !empty($pass) && $oConf->loginCheck($user, $pass ,$message )){
		//ログイン成功時
		header('Location: ./index.php?root='.$_SESSION['def_name'].'');
// 		header('Location: ./index.php?root='.$_SESSION['def_name'].'');
		exit;
	}else {
		if(isset($sani['mess'])){
			$message = mb_convert_encoding($sani['mess'], "SJIS", "UTF-8, JIS, sjis-win");
		}
		require_once('./view/vw_index.php');
		exit;
	}
} elseif($strType === $oConf->getKind(0)){
//種別がPMのとき
//ログインできていなければ初期画面に戻す
	//両方の関数を通すために全体の否定
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//ログイン成功時
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}
	$fp = fopen($fileName,'r');
	$title = '';
	if(!$fp){
		$oLog->error('[target file not found]'.$fileName.':'.__FILE__.':'.__LINE__);
		$message = '対象のメニューがありませんでした';//メッセージを差換え
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId     = $_SESSION['user_id'];
	require_once('./lib/clsProcInfo.php');

	$oProcs = New clsProcInfo('runExec',$userId);

	while(!feof($fp)){
// 		$temp = mb_convert_encoding(fgets($fp),'utf8','sjis');
		$temp = fgets($fp);
		//RUNの後ろのPG名称とそのFILを取得どこかで使える？
		$pgArray = array();
		$strFil = "";
		//メニューのタイトルの取得
		if(preg_match('/^([0]{2}.*)/m',$temp)){
			//SJISだと問題が出るのでUTFで
// 			preg_match('/^[0-9]{2}\s(.*(JIP)? || .{1,50})/m',$temp,$pgArray);
			if(preg_match('/^[0-9]{2}\s.{1,50}JIP/m',$temp)){
				preg_match('/^[0-9]{2}\s(.*)JIP/m',$temp,$pgArray);
			}else{
				preg_match('/^[0-9]{2}\s(.{1,50})/m',$temp,$pgArray);
			}
			$title = $pgArray[1];
		}
		//次メニューの項目の取得
		if(preg_match('/(^[1-9][0-9]|^0[1-9])\s.{6}/m',$temp)){
			preg_match('/^([0-9]{2})\s(.{6})(.{2})(.{30}[^\s]*)/m',$temp,$pgArray);
			//次のファイル名は空白文字を除去する必要あり
// 			$pgArray[2] = trim($pgArray[2]);
// 			array_push($arrSelectItem,$pgArray);
			array_push($arrSelectItem,array_map("rtrim",$pgArray));
		}
	}
	$result = New clsMenuItem($title);

	//ステータスバー対応　メニュー画面時の表示
	$jobName = substr($fileName,8);
	$jobName = substr($jobName,0,-3);
	$jobName = mb_strtoupper($jobName);

	require_once('./view/vw_menu.php');
	exit;
}elseif($strType === $oConf->getKind(1)){
//種別がJSだったとき
//ログインできていなければ初期画面に戻す
	//両方の関数を通すために全体の否定
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//ログイン失敗時
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId     = $_SESSION['user_id'];

	require_once('./lib/clsScreen.php');
	require_once('./lib/clsBackGroundProcess.php');
	require_once('./lib/clsProcInfo.php');

	$oProcs = New clsProcInfo('runExec',$userId);

	//ERROR出力用にファイル名を追加
	$cmd = $fileName;

	$screen = New clsScreen();
	$clsBP = New BackgroundProcess($cmd,$userId);
	$oLog->info(__FILE__.':'.__LINE__.'[cmd]:'.$cmd);
	$clsBP->run();
	$result = $clsBP->pRead();

	$screen->screenParse($result);

// 	var_dump($result);
	require_once('./view/vw_exec.php');
	exit;
}elseif($strType === $oConf->getKind(2)){
//種別がLMだったとき
//ログインできていなければ初期画面に戻す
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//ログイン失敗時
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId = $_SESSION['user_id'];

	require_once('./lib/clsScreen.php');
	require_once('./lib/clsBackGroundProcess.php');
	require_once('./lib/clsProcInfo.php');

	$oProcs = New clsProcInfo('run',$userId);

	$cmd = $fileName;
	$screen = New clsScreen();
	$clsBP = New BackgroundProcess($cmd,$userId);
	$clsBP->run();
	require_once('./view/vw_exec.php');
	exit;
}elseif($strType === $oConf->getKind(3)){
//種別がSMだったとき
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//ログイン失敗時
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}
	//Smart用のClassを準備
	require_once('./lib/clsSmartParse.php');
	$clsSmart =  New clsSmartParse($fileName);
	$title = '';
	$title = str_replace(' ','&nbsp;',$sani['title']);
	if(!$clsSmart->fp){
		$oLog->error('[target file not found]'.$fileName.':'.__FILE__.':'.__LINE__);
		$message = '対象がありませんでした';//メッセージを差換え
		header('Location: ./index.php');
		exit;
	}

	require_once('./view/vw_smexec.php');
	exit;
}

?>
