<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post          = $_REQUEST;
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
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
// 	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
	exit;
}

//���ڈꗗ�擾
//upd koyama ��Œǉ���Ajax�Ŏ擾����
if(!empty($post) && array_key_exists('num',$post)){
	get_items($items,$message_codes,$dbhNIS,($post['num']));
}else{
	get_items($items,$message_codes,$dbhNIS,0);
}
//��ʕ`��d
require_once('./view/vw_item_conf_listadd_ajax.php');

?>
