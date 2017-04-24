<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post           = $_POST;
$tables         = array(); //�|�X�g�f�[�^����e�[�u����
$selectedBonds  = array(); //�߂��Ă����Ƃ��̌�������
$selectedTables = array();
$message_codes  = '';

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
	if(isset($post['selectedBonds'])){
		//�f�[�^�����ϊ�
		foreach($post['selectedBonds'] as $tmp){
			//AHNHF.AHNH-R~~1~64~~LEFTJOIN~B-TCM.BM-KEY~~1~4~
			$tmps  = explode("~", $tmp);//�u~�v�ŕ���
			$ltmps = explode(".", $tmps[0]);
			$rtmps = explode(".", $tmps[6]);
			$add   = array();
			$add['lListTableName']     = trim($ltmps[0]);
			$add['lListItemName']      = trim($ltmps[1]);
			if(trim($tmps[1]) == ""){
				$add['lListJapaneseName']  = " "; //�X�y�[�X��ݒ肷�邱�Ƃŕ\�����ɂ́u&nbsp;�v�ƕ\�������
			}else{
				$add['lListJapaneseName']  = trim($tmps[1]);
			}
			$add['lListS_point']       = trim($tmps[2]);
			$add['lListSize']          = trim($tmps[3]);
			$add['lListType']          = trim($tmps[4]);
			if($tmps[5]=="LEFTJOIN"){
				$add['bondConditions']     = "�������Ɍ���";
			}else{
				$add['bondConditions']     = "�݂��Ɉ�v�̂�";
			}
			$add['rListTableName']     = trim($rtmps[0]);
			$add['rListItemName']      = trim($rtmps[1]);
			if(trim($tmps[7]) == ""){
				$add['rListJapaneseName']  = " "; //�X�y�[�X��ݒ肷�邱�Ƃŕ\�����ɂ́u&nbsp;�v�ƕ\�������
			}else{
				$add['rListJapaneseName']  = trim($tmps[7]);
			}
			$add['rListS_point']       = trim($tmps[8]);
			$add['rListSize']          = trim($tmps[9]);
			$add['rListType']          = trim($tmps[10]);
			$add['key']                = trim($tmps[0]) ."~".trim($tmps[6]);
			$add['id']                 = trim($tmp);
			array_push($selectedBonds, $add);
		}
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501',$e);
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//��ʕ`��
require_once('./view/vw_DataView_Free_Search_2.php');

?>
