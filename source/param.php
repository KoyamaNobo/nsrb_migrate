<?php
if(!empty($_POST)){
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/config.php');
	$sani = $_POST;
	if(!empty($sani['infname']) && !empty($sani['outfname'])){
		require_once('./lib/clsScreen.php');
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
<meta http-equiv="content-language" content="ja" />
<title><?php echo SITE_TITLE ?></title>
</head>
<?php
		$clsAP = New AsynchronousProcess($sani['infname'],$sani['outfname']);
// 		$clsAP->oLog->info(__FILE__.':'.__LINE__.':Value '.print_r($sani,true));
		$oLog   = New Log('');
		$screen = New clsScreen();
		$clsAP->pWrite($sani['value']);
		
		usleep(WRITE_WAIT);
		$result = $clsAP->pRead();
		$screen->screenParse($result);
		foreach($screen->arrScreenStr as $key=>$view){
			echo $view->strStartTag.PHP_EOL;
			//echo表示がなければ中身の表示を試す
			if(empty($view->echoElem)){
				if(count($view->arrLineElem) > 0){
					foreach($view->arrLineElem as $key=>$elem){
						//行を削除した時に削除されている可能性ありｂ
						if(isset($elem)){
							//文字列の表示
							if($elem->type == 'TEX'){
								echo $elem->text.PHP_EOL;
							}
							//入力項目の表示
							if($elem->type == 'INP'){
								echo $elem->text.PHP_EOL;
							}
						}
					}
				}
			}else{
				echo $view->echoElem.PHP_EOL;
			}
			echo $view->strEndTag.PHP_EOL;
			if(count($view->arrLineElem) > 0){
				foreach($view->arrLineElem as $key=>$elem){
					//行を削除した時に削除されている可能性ありｂ
					if(isset($elem)){
						//罫線(下線)
						if($elem->type == 'UND'){
							echo $elem->text.PHP_EOL;
						}
						//罫線(箱)
						if($elem->type == 'BOX'){
							echo $elem->text.PHP_EOL;
						}
						//罫線(縦線)
						if($elem->type == 'VER'){
							echo $elem->text.PHP_EOL;
						}
						//罫線(上線)
						if($elem->type == 'OVE'){
							echo $elem->text.PHP_EOL;
						}
					}
				}
			}
		}
		if(count($screen->execErrorArray) > 0){
			foreach($screen->execErrorArray as $error){
?>
				<input type="hidden" class="error"  value="<?php echo $error; ?>">
<?php
			}
		}
		
		//プロセス終了時の戻る対応
		//プロセス終了時の戻る対応（指定のプロセスIDが実行中かチェック）
		$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
		//指定のプロセスIDが実行中かチェック
		if($state==false){
?>
				<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">
<?php
		}
?>
</body>
</html>
<?php
	}
}else{
	echo "Bad Request";
}
?> 