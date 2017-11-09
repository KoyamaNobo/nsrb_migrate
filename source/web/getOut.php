<?php
session_start();
if(!empty($_POST)){
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/config.php');
	$sani = $_POST;
	if(!empty($sani['infname']) && !empty($sani['outfname'])){
		require_once('./lib/clsScreen.php');
		require_once('./lib/log.php');
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?php
		$oLog   = New Log('');
		$clsAP  = New AsynchronousProcess($sani['infname'],$sani['outfname']);
		$screen = New clsScreen();
		// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
		$screen->screenParse($clsAP->pRead());

		//プロセス終了時の戻る対応
		//指定のプロセスIDが実行中かチェック
		$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
		// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
		if($state==false){
			//終了している場合は以下のinputを出力
			echo '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
		}
	}
}else{
	echo "Bad Request";
}
?>
