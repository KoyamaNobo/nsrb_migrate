<?php
//cmd�������Ƃ��͒��g�����ׂĖ���
//if(!empty($sani['root_cmd'])){
	//������肪�N�������Ƀf�t�H���g�̃��j���[
	$sani['root'] = $_SESSION['def_name'];
	//cmd�������Ă���Ƃ���SESSION������
	if(!empty( $_SESSION['user_name']) || !empty($_SESSION['password']) ){
		//���łɃ��O�C�����Ă���΍ă��O�C�����Ȃ��d�l
		$user = $_SESSION['user_name'];
		$pass = $_SESSION['password']; 
	}else{
		$user = '';
		$pass = '';
	}
	if(!$oConf->loginCheck($user, $pass ,$message )){
		//���s
		if(isset($sani['mess'])){
			$message = $sani['mess'];
		}
		require_once('./rtView/vw_index.php');
		exit;
	}
	require_once('./lib/clsExecScreen.php');
	$termFlg = true;
	
	//��ʂɕ\�������邽�߁A��ʂ��I�u�W�F�N�g�𗬗p
	require_once('./rtView/vw_roexec.php');
	exit;
	$oLog->info('file:'.__FILE__.':'.__LINE__.' targfile:'.SOURCEDIR.$strMenuDir.$sani['root'].'.sh');
//}
?>