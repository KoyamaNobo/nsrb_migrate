<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post               = $_POST;
$selectedBonds      = array();
$selectedItemNames  = array();
$selectedTables     = array();
$message_codes       = '';

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
	//�\�����ڎ擾�i�f�[�^�ϊ��j
	if(isset($post['selectedItemNames'])){
		foreach($post["selectedItemNames"] as $tmp){
			$tmps = explode("~", $tmp);
			$add = array();
			$add["listTableName"]      = $tmps[0];
			$add["listItemName"]       = $tmps[1];
			if($tmps[2] == ""){
				$add["listJapaneseName"]   = " "; //�󂾂�����X�y�[�X�����邱�ƂŁu&nbsp;�v�ɕϊ������
			}else{
				$add["listJapaneseName"]   = $tmps[2];
			}
			$add["listId"]             = $tmps[3];
			$add["listS_point"]        = $tmps[4];
			$add["listSize"]           = $tmps[5];
			$add["listType"]           = $tmps[6];
			$add["id"]                 = $tmps[3];
			array_push($selectedItemNames, $add);
		}
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//��ʕ`��
require_once('./view/vw_DataView_Free_Search_3.php');

?>
