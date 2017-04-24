<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post              = $_POST;
$selectedBonds     = array();
$selectedItemNames = array();
$selectedFilters   = array();
$selectedTables    = array();
$message_codes     = '';
$listTypeFlg       = true; //vw_Part_DataView_ItemNames�Ŏg�p

//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

if(!empty($post)){
	//�I���e�[�u���擾
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}
	//���������擾
	foreach($post["selectedBonds"] as $tmp){
		array_push($selectedBonds, $tmp);
	}
	//�\�����ڎ擾
	foreach($post["selectedItemNames"] as $tmp){
		array_push($selectedItemNames, $tmp);
	}
	//���o�����擾
	if(isset($post["selectedFilters"])){
		foreach($post["selectedFilters"] as $tmp){
			array_push($selectedFilters, $tmp);
		}
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//��ʕ`��
require_once('./view/vw_DataView_Free_Search_5.php');

?>
