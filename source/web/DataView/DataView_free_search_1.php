<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$tables            = array();                               //DB���e�[�u�����̈ꗗ�擾�̌��ʂ�����
$post              = $_POST;                                //post�l���i�[�i�߂�{�^���őJ�ڂ��Ă����Ƃ��j
$selectedTables    = array();                               //�I���e�[�u�����i�[
$message_codes     = '';                                    //���b�Z�[�W���i�[

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

//POST�l�擾
if(!empty($post)){
	//�߂�őJ�ڂ����Ƃ�
	//�I���e�[�u���擾
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}
}

//DB���e�[�u�����̈ꗗ�擾
get_all_tables($tables,$message_codes,$dbhNIS);

//��ʕ`��
require_once('./view/vw_DataView_Free_Search_1.php');

?>
