<?php
//�Z�b�V�����X�^�[�g
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post         = $_POST;                                //post�l���i�[
$get          = $_GET;                                 //get�l���i�[
$loginFlg     = false;                                 //���j���[��ʂɑJ�ڂ��Ă��悢���̃t���O���i�[
$message_codes = "";                                   //�G���[���b�Z�[�W�R�[�h���i�[

//post�l�擾
if(!empty($post['dv_user_id'])){
	//���O�C���{�^��������
	//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
	if(db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
		//DB�ڑ��ɐ��������ꍇ
		//���O�C���`�F�b�N
		if(loginCheck($post['dv_user_id'], $post['dv_password'],$message_codes,$dbhDV)){
			//���O�C���`�F�b�N�ɐ��������ꍇ
			$loginFlg = true;
		}
	}
}else if(!empty($get['message'])){
	//���_�C���N�g�Ŗ߂��Ă����ꍇ�i�s�������j
	$message_codes = $get['message'];
}

//��ʕ`��
if($loginFlg){
	require_once('./view/vw_DataView_Menu.php');
}else{
	require_once('./view/vw_DataView_Login.php');
}

?>
