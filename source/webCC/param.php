<?php
if(!empty($_POST)){
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/config.php');
	$sani = $_POST;
	if(!empty($sani['infname']) && !empty($sani['outfname'])){
		require_once('./lib/clsScreen.php');
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?php
	$clsAP = New AsynchronousProcess($sani['infname'],$sani['outfname']);
// 		$clsAP->oLog->info(__FILE__.':'.__LINE__.':Value '.print_r($sani,true));
	$oLog   = New Log('');
	$screen = New clsScreen();

	$clsAP->pWrite($sani['value']);
	$statArray = getProcessIdStatus($sani['pid'],$oLog);
	$screen->screenParse($clsAP->pRead());
?>
<?php
	//プロセス終了時の戻る対応
	//プロセス終了時の戻る対応（指定のプロセスIDが実行中かチェック）
	$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
	//指定のプロセスIDが実行中かチェック
	if($state==false){
?>
	<input id="line_index" type="hidden" name="line_index" value="<?php if(!empty($clsAP->getline_index)){ echo $clsAP->getline_index;}else{echo '0';} ?>" />
	<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">
<?php
	}

	//ステータスバー対応
	//・メッセージ
	$errstr = "";
	if(count($screen->execErrorArray) > 0){
		foreach($screen->execErrorArray as $error){
			//「 Error 」の文字列を消す
			if (preg_match("/ Error /", $error)) {
				$tmpstr = substr($error, 7);
			}else{
				$tmpstr = $error;
			}
			//40幅に収まる範囲まで文字列を取得
			$tmpstr2 = "";
			if(strlen($tmpstr) > 0){
				$roopCount = 0;
				for ($roopCount = 0; $roopCount < strlen($tmpstr); $roopCount++) {
					$tmpstr2 = $tmpstr2 . substr($tmpstr, $roopCount, 1);
					$len = mb_strwidth($tmpstr2);
					if($len > 40){
						break;
					}
				}
			}
?>
<input id="status1Get" type="hidden" class="statusGet"  value="' . $tmpstr2 . '">
<?php
		}
	}else{
		//エラーがない時も出力すれば切り替わる
?>
<input id="status1Get" type="hidden" class="statusGet"  value="">
<?php
	}

	//・プログラムの状態その１
	if($statArray['stat'] != '' ){
?>
<input id="status2Get" type="hidden" class="statusGet" value="<?php echo $statArray['stat'];?>">
<?php
	}
	//・実行中のプログラム名
	if($statArray['jobname'] != '' ){
?>
<input id="status4Get" type="hidden" class="statusGet" value="<?php echo $statArray['jobname'];?>">
<?php
	}
?>
<?php
	}
}else{
	echo "Bad Request";
}
?>
