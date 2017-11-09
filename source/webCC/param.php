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
	//�v���Z�X�I�����̖߂�Ή�
	//�v���Z�X�I�����̖߂�Ή��i�w��̃v���Z�XID�����s�����`�F�b�N�j
	$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
	//�w��̃v���Z�XID�����s�����`�F�b�N
	if($state==false){
?>
	<input id="line_index" type="hidden" name="line_index" value="<?php if(!empty($clsAP->getline_index)){ echo $clsAP->getline_index;}else{echo '0';} ?>" />
	<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">
<?php
	}

	//�X�e�[�^�X�o�[�Ή�
	//�E���b�Z�[�W
	$errstr = "";
	if(count($screen->execErrorArray) > 0){
		foreach($screen->execErrorArray as $error){
			//�u Error �v�̕����������
			if (preg_match("/ Error /", $error)) {
				$tmpstr = substr($error, 7);
			}else{
				$tmpstr = $error;
			}
			//40���Ɏ��܂�͈͂܂ŕ�������擾
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
		//�G���[���Ȃ������o�͂���ΐ؂�ւ��
?>
<input id="status1Get" type="hidden" class="statusGet"  value="">
<?php
	}

	//�E�v���O�����̏�Ԃ��̂P
	if($statArray['stat'] != '' ){
?>
<input id="status2Get" type="hidden" class="statusGet" value="<?php echo $statArray['stat'];?>">
<?php
	}
	//�E���s���̃v���O������
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
