<?php
//
//gmenu�p�e�X�g���
//20131125 create koyama
require_once('./lib/config.php');
require_once('./lib/clsMenuItem.php');
require_once('./lib/log.php');
session_start();

$fileName      = DEFFNAME;
$strType       = DEFKIND;                          //�����̃^�C�v���i�[
$oConf         = New initConf();                    //config.php���R���t�B�O�p�N���X
$oLog          = New Log('');                      //���M���O�N���X
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

//*************************************************************************���͂ɂ��TYPE�̐؂蕪��
//textbox�ɉ������͂����ꍇ?
if(!empty($sani['root'])){
	//�t�@�C���̑��݂��m�F�ł�����Q�Ɛ�����ւ�
	//$arrMenuDir[0]�̓��j���[�̃t�H���_�ł���z��@�d�l�ύX���͗p�m�F
// 	$oLog->info('file:'.__FILE__.' targfile:'.SOURCEDIR.$strMenuDir.$sani['root'].'.sh');
	if(file_exists(SOURCEDIR.$strMenuDir.strtolower($sani['root']).'.sh')){
		$fileName = SOURCEDIR.$strMenuDir.strtolower($sani['root']).'.sh';
		$strType = 'PM';
	}
}

//$fileName = SOURCEDIR.$fileName;
//�����N���痈���ꍇ?
if(!empty($sani['filename']) && empty($sani['root'])){
	if(isset($sani['typ'])){
		//MENU
		switch ($sani['typ']) {
			case strtolower($oConf->getKind(0)):
				//�t�@�C���̑��݂��m�F�ł�����Q�Ɛ�����ւ�////////PM
				if(file_exists(SOURCEDIR.$strMenuDir.$sani['filename'].'.sh')){
					$fileName = SOURCEDIR.$strMenuDir.$sani['filename'].'.sh';
					$strType = $oConf->getKind(0);
				}
				break;
			case strtolower($oConf->getKind(1)):
				//�t�@�C���̑��݂��m�F�ł�����Q�Ɛ�����ւ�//////JS
				if(file_exists(SOURCEDIR.$arrJclDir.$sani['filename'].'.sh')){
					$fileName = SOURCEDIR.$arrJclDir.$sani['filename'].'.sh';
					$strType = $oConf->getKind(1);
				}
				break;
			case strtolower($oConf->getKind(2)):
				//�t�@�C���̑��݂��m�F�ł�����Q�Ɛ�����ւ�        //LM
				$sani['filename'] = strtoupper($sani['filename']);
				if(file_exists(SOURCEDIR.$arrLmDir.$sani['filename'].'')){
					$fileName = SOURCEDIR.$arrLmDir.$sani['filename'].'';
					$strType = $oConf->getKind(2);
				}
				break;
			case strtolower($oConf->getKind(3)):
				//�t�@�C���̑��݂��m�F�ł�����Q�Ɛ�����ւ�        //LM
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

//*************************************************************************procInfo�p�̐ݒ�
if(array_key_exists('procInfo',$sani)){
//��ʂ�LM�������Ƃ�
//���O�C���ł��Ă��Ȃ���Ώ�����ʂɖ߂�
	$oLog->info('[procInfo execute]'.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password'].__FILE__.':'.__LINE__);
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C�����s��
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
	//�v���Z�X�̏�Ԏ擾
	//TODO:������pid�����Ȃ��Ə��������������Ȃ� koyama 20161018
	//upd koyama 20161018 Pid�����Ȃ�
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
//*************************************************************************TYPE�ɂ��\���̐؂蕪��
// var_dump($strType);
// $oLog->info('file:'.__FILE__.':'.__LINE__.' var:'.print_r($sani,true).$strType);
if($strType === DEFKIND){
	//��ʂ������l(�󕶎���)�̂Ƃ�
	//POST�p�����[�^�������Ă��Ȃ��Ƃ���SESSION�̒l�Ń��O�C�����g���C
	if(!empty($_POST['account']) || !empty($_POST['passwd']) ){
		$user = $_POST['account'];
		$pass = $_POST['passwd'];
	} elseif(!empty( $_SESSION['user_name']) || !empty($_SESSION['password']) ){
		//���łɃ��O�C�����Ă���΍ă��O�C�����Ȃ��d�l
		$user = $_SESSION['user_name'];
		$pass = $_SESSION['password'];
	}else{
		$user = '';
		$pass = '';
	}
	if(!empty($user) && !empty($pass) && $oConf->loginCheck($user, $pass ,$message )){
		//���O�C��������
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
//��ʂ�PM�̂Ƃ�
//���O�C���ł��Ă��Ȃ���Ώ�����ʂɖ߂�
	//�����̊֐���ʂ����߂ɑS�̂̔ے�
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C��������
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}
	$fp = fopen($fileName,'r');
	$title = '';
	if(!$fp){
		$oLog->error('[target file not found]'.$fileName.':'.__FILE__.':'.__LINE__);
		$message = '�Ώۂ̃��j���[������܂���ł���';//���b�Z�[�W��������
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId     = $_SESSION['user_id'];
	require_once('./lib/clsProcInfo.php');

	$oProcs = New clsProcInfo('runExec',$userId);

	while(!feof($fp)){
// 		$temp = mb_convert_encoding(fgets($fp),'utf8','sjis');
		$temp = fgets($fp);
		//RUN�̌���PG���̂Ƃ���FIL���擾�ǂ����Ŏg����H
		$pgArray = array();
		$strFil = "";
		//���j���[�̃^�C�g���̎擾
		if(preg_match('/^([0]{2}.*)/m',$temp)){
			//SJIS���Ɩ�肪�o��̂�UTF��
// 			preg_match('/^[0-9]{2}\s(.*(JIP)? || .{1,50})/m',$temp,$pgArray);
			if(preg_match('/^[0-9]{2}\s.{1,50}JIP/m',$temp)){
				preg_match('/^[0-9]{2}\s(.*)JIP/m',$temp,$pgArray);
			}else{
				preg_match('/^[0-9]{2}\s(.{1,50})/m',$temp,$pgArray);
			}
			$title = $pgArray[1];
		}
		//�����j���[�̍��ڂ̎擾
		if(preg_match('/(^[1-9][0-9]|^0[1-9])\s.{6}/m',$temp)){
			preg_match('/^([0-9]{2})\s(.{6})(.{2})(.{30}[^\s]*)/m',$temp,$pgArray);
			//���̃t�@�C�����͋󔒕�������������K�v����
// 			$pgArray[2] = trim($pgArray[2]);
// 			array_push($arrSelectItem,$pgArray);
			array_push($arrSelectItem,array_map("rtrim",$pgArray));
		}
	}
	$result = New clsMenuItem($title);

	//�X�e�[�^�X�o�[�Ή��@���j���[��ʎ��̕\��
	$jobName = substr($fileName,8);
	$jobName = substr($jobName,0,-3);
	$jobName = mb_strtoupper($jobName);

	require_once('./view/vw_menu.php');
	exit;
}elseif($strType === $oConf->getKind(1)){
//��ʂ�JS�������Ƃ�
//���O�C���ł��Ă��Ȃ���Ώ�����ʂɖ߂�
	//�����̊֐���ʂ����߂ɑS�̂̔ے�
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C�����s��
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}

	$userId     = $_SESSION['user_id'];

	require_once('./lib/clsScreen.php');
	require_once('./lib/clsBackGroundProcess.php');
	require_once('./lib/clsProcInfo.php');

	$oProcs = New clsProcInfo('runExec',$userId);

	//ERROR�o�͗p�Ƀt�@�C������ǉ�
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
//��ʂ�LM�������Ƃ�
//���O�C���ł��Ă��Ȃ���Ώ�����ʂɖ߂�
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C�����s��
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
//��ʂ�SM�������Ƃ�
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C�����s��
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}
	//Smart�p��Class������
	require_once('./lib/clsSmartParse.php');
	$clsSmart =  New clsSmartParse($fileName);
	$title = '';
	$title = str_replace(' ','&nbsp;',$sani['title']);
	if(!$clsSmart->fp){
		$oLog->error('[target file not found]'.$fileName.':'.__FILE__.':'.__LINE__);
		$message = '�Ώۂ�����܂���ł���';//���b�Z�[�W��������
		header('Location: ./index.php');
		exit;
	}

	require_once('./view/vw_smexec.php');
	exit;
}

?>
