<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post              = $_POST;
$selectedBonds     = array();
$selectedItemNames = array();
$selectedFilters   = array();
$selectedSorts     = array();
$selectedTables    = array();
$message_codes     = '';
$listTypeFlg       = false; //vw_Part_DataView_ItemNames�Ŏg�p

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

	//��������
	foreach($post["selectedBonds"] as $tmp){
		array_push($selectedBonds, $tmp);
	}

	//�\������
	foreach($post["selectedItemNames"] as $tmp){
		array_push($selectedItemNames, $tmp);
	}

	//���o����
	if(isset($post["selectedFilters"])){
		foreach($post["selectedFilters"] as $tmp){
			array_push($selectedFilters, $tmp);
		}
	}

	//���בւ��ݒ�
	if(isset($post["selectedSorts"])){
		foreach($post["selectedSorts"] as $tmp){
			$tmps = explode("~", $tmp);
			$add = array();

			//AHNHF~AHNH-KEY~~asc~1~7~~AHNHF

			$add["listTableName"]      = $tmps[0];
			$add["listItemName"]       = $tmps[1];
			$add["listJapansesName"]   = $tmps[2];
			$add["listId"]         = $tmps[7] . "." . $tmps[1];
			if($tmps[3] == "asc"){
				$add["listSort"]       = "����";
			}else{
				$add["listSort"]       = "�~��";
			}
			$add["listS_point"]    = $tmps[4];
			$add["listSize"]       = $tmps[5];
			$add["listType"]       = $tmps[6];
			$add["id"]             = $tmps[7].".".$tmps[1];
			$add["listNext"]       = $tmp;
			array_push($selectedSorts, $add);
		}
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}



//��ʕ`��
require_once('./view/vw_DataView_Free_Search_6.php');

?>
