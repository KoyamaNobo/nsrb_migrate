<?php
//
//gmenu�p�e�X�g���
//20170203 create koyama
session_start();
require_once('./lib/config.php');
require_once('./lib/clsMenuItem.php');
require_once('./lib/log.php');

$fileName      = DEFFNAME;
$strType       = DEFKIND;                          //�����̃^�C�v���i�[
$oConf         = New initConf();                    //config.php���R���t�B�O�p�N���X
$oLog          = New Log('');                      //���M���O�N���X
$oProcs;
$temp          = '';
$arrResult     = array();

$sani = $_GET;
$message = "";
if(!empty($sani['mess'])){
	$message = $sani['mess'];
}

//*************************************************************************���͂ɂ��TYPE�̐؂蕪��
//��ʂ�LM�������Ƃ�����ڐA����Start
//���O�C���ł��Ă��Ȃ���Ώ�����ʂɖ߂�
	if(!(sessionCheck($message) && $oConf->loginCheck($_SESSION['user_name'], $_SESSION['password'] ,$message ))){
		//���O�C�����s��
		$oLog->error('[not found user_id]'.__FILE__.':'.__LINE__.':'." un:".$_SESSION['user_name']." ps:". $_SESSION['password']);
		header('Location: ./sessDestroy.php?mess='.$message.'');
		exit;
	}
	$userId = $_SESSION['user_id'];
	require_once('./lib/clsProcInfo.php');
	$oProcs = New clsProcInfo('run',$userId);

	$arrResult['procNum'] = $oProcs->getCountProcArray();
	//���s���̃��j���[����\��
	$arrResult['procList'] = $oProcs->getHTMLToProcs();
	echo $oProcs->getHTMLToProcs();
	exit;
?>
