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
			//echo�\�����Ȃ���Β��g�̕\��������
			if(empty($view->echoElem)){
				if(count($view->arrLineElem) > 0){
					foreach($view->arrLineElem as $key=>$elem){
						//�s���폜�������ɍ폜����Ă���\�����肂
						if(isset($elem)){
							//������̕\��
							if($elem->type == 'TEX'){
								echo $elem->text.PHP_EOL;
							}
							//���͍��ڂ̕\��
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
					//�s���폜�������ɍ폜����Ă���\�����肂
					if(isset($elem)){
						//�r��(����)
						if($elem->type == 'UND'){
							echo $elem->text.PHP_EOL;
						}
						//�r��(��)
						if($elem->type == 'BOX'){
							echo $elem->text.PHP_EOL;
						}
						//�r��(�c��)
						if($elem->type == 'VER'){
							echo $elem->text.PHP_EOL;
						}
						//�r��(���)
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
		
		//�v���Z�X�I�����̖߂�Ή�
		//�v���Z�X�I�����̖߂�Ή��i�w��̃v���Z�XID�����s�����`�F�b�N�j
		$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
		//�w��̃v���Z�XID�����s�����`�F�b�N
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