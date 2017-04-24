<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');
//��p���C�u�����ǂݍ���
require_once('./lib/clsDBM_Extension.php');

//�ϐ�������
$post          = $_POST;
$TableName     = '';
$ItemName      = '';
$Japanese_Name = '';
$NonDisp_Flg   = '';
$S_point       = '';
$Size          = '';
$Disp_Num      = '';
$today         = date("Y-m-d H:i:s");
$Cre_Date      = $today;
$Mod_Date      = $today;
$Del_Flg       = '0';
$items         = array();
$successCode   = '';
$message_codes = '';

//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

$clsData = New clsDBM_Extension($dbhDV,$_SESSION['dv_user_id'],DB_NAME,$_SESSION['dv_user_disp_num']);

//�C���T�[�gor�A�b�v�f�[�g����
if(!empty($post)){
	//�󂯎�����p�����[�^�����W���[���ɓn��
	$clsData->setState($post);

	//post�Ŏ󂯎�����������ɂ����s�����擾
	$loopCount = $clsData->getRowCountWithState();
	if(array_key_exists('action',$post) && preg_match('/^del.*/',$post['action']) == 1){
		//���݂��Ă���Ƃ��̓A�b�v�f�[�g
		$success = 0;
		$success = $clsData->delData();
		if($success){
			//�폜������
			$successCode   = 'INF0007';
		}else{
			//�폜���s��
			$message_codes = 'ERR0020';
			put_error($message_codes,'');
		}
	}else if($loopCount != 0){
		//���݂��Ă���Ƃ��̓A�b�v�f�[�g
		$success = 0;
		$success = $clsData->updateData();
		if($success){
			//�C���T�[�g������
			$successCode   = 'INF0008';
		}else{
			//�C���T�[�g���s��
			$message_codes = 'ERR0017';
			put_error($message_codes,'');
		}

	}else{
		//���݂��Ă��Ȃ��Ƃ��̓C���T�[�g
		$success = 0;
		//�C���T�[�g�������������ǂ���
		$success = $clsData->insertData();

		if($success){
			//�C���T�[�g������
			$successCode   = 'INF0008';
		}else{
			//�C���T�[�g���s��
			$message_codes = 'ERR0018';
			put_error($message_codes,'');
		}

	}
}
//���ڈꗗ�擾
//upd koyama ��Œǉ���Ajax�Ŏ擾����
$clsData->get_items(0);

//��ʕ`��d
require_once('./view/vw_users_item.php');

?>
