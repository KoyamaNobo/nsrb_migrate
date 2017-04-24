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
		$clsAP = New AsynchronousProcess($sani['infname'],$sani['outfname']);
		$screen = New clsScreen();
		$oLog   = New Log('');

		//プロセスの状態取得
		$statArray = getProcessIdStatus($sani['pid'],$oLog);

		$screen->screenParse($clsAP->pRead());

		//プロセス終了時の戻る対応
		//指定のプロセスIDが実行中かチェック
		$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
		if($state==false){
			//終了している場合は以下のinputを出力
			echo '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
		}
?>
	<input id="status2Get" type="hidden" class="statusGet" value="<?php echo $statArray['stat'];?>">
<?php
		//・実行中のプログラム名
		if($statArray['jobname'] != '' ){
?>
	<input id="status4Get" type="hidden" class="statusGet" value="<?php echo $statArray['jobname'];?>">
<?php
		}
	}
}else{
	echo "Bad Request";
}
?>
