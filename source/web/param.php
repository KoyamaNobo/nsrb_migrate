<?php
// FIXME �f�o�b�O�p�w�b�_�[�B��ō폜����B
header('Access-Control-Allow-Origin: *');

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

	// �f�[�^�������݁����������܂őҋ@
	if (is_array ($sani['value'])) {
		// �p�����[�^�������w�肳��Ă���ꍇ
		$count = count($sani['value']);
		for ($i = 0 ; $i < $count - 1; $i++) {
			$value = $sani['value'][$i];
			$clsAP->pWriteAndReadWait($value);
		}

		// �Ō�͏o�͌��ʂ�������܂őҋ@
		$clsAP->pWriteAndProcWait($sani['value'][$count - 1]);
	} else {
		$clsAP->pWriteAndProcWait($sani['value']);
	}

	list($time, $data) =  $clsAP->pReadAndTime();

	//�v���Z�X�I�����̖߂�Ή��i�w��̃v���Z�XID�����s�����`�F�b�N�j
	$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
	//�w��̃v���Z�XID�����s�����`�F�b�N
	if($state==false){
		$data .= '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
	}
	// �f�[�^�̃^�C���X�^���v�𖖔��ɕt�^
	$data .= '<input type="hidden" id="dataTimestamp" value="' . $time . '" />';

	$screen->screenParse($data);

	}
}else{
	echo "Bad Request";
}
?>
