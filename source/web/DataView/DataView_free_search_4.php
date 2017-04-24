<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//���o���������֐�
function sfSprit(&$selectedFilters,$postFilters){
	foreach($postFilters as $tmp){
		$tmps = explode("~", $tmp);
		$add = array();
		$add["andOr"]        = $tmps[0];
		$add["tableName"]    = $tmps[1];
		$add["itemName"]     = $tmps[2];
		if($tmps[3]==""){
			$add["JapaneseName"] = " "; //�󂾂������X�y�[�X�������邱�Ƃŕ\�����Ɂu&nbsp;�v�ɕϊ������
		}else{
			$add["JapaneseName"] = $tmps[3];
		}
		$add["s_point"]      = $tmps[4];
		$add["size"]         = $tmps[5];
		$add["type"]         = $tmps[6];
		$add["operator"]     = $tmps[7];
		$add["value"]        = $tmps[8];
		$add["id"]           = $tmps[9];
		array_push($selectedFilters, $add);
	}
}

//�ϐ�������
$post              = $_POST;
$selectedBonds     = array();
$selectedItemNames = array();
$selectedFilters   = array();
$selectedTables    = array();
$sf                = array();
$message_codes     = '';


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
	//�I���e�[�u��
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

	//���o����(�߂��ʂ���߂��Ă����ꍇ�j
	if(!empty($post["selectedFilters"])){
		sfSprit($selectedFilters,$post["selectedFilters"]);
	}
	//���o�����i�ǉ���ʂ���J�ڂ����ꍇ�j
	if(!empty($post["selectedFilter"])){
		sfSprit($selectedFilters,$post["selectedFilter"]);
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//��ʕ`��
require_once('./view/vw_DataView_Free_Search_4.php');

?>
