<?php
if(!isset($_SESSION)){
	session_start();
}

//���O�t�@�C���ǂݍ���
require_once('./lib/log.php');
require_once('./lib/config.php');

//changeHttps();

//�ϐ���`
$get = $_GET;
$post = $_POST;
$file = "";
$messages = array();
$updateerr="0";

if(!isset($oConf)){
	$oConf	= New initConf();					//config.php���R���t�B�O�p�N���X
}

//post�������Ƃ���
if(!isset($post) || empty($post)){
	$file['name'] = $get['filename'];
	$file['name_befor'] = $get['filename'];
}else{
	$file['name'] = $post['filename'];
	$file['name_befor'] = $post['filename_befor'];
}

//�Z�b�V�����m�F
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//�G���[����
	$mess = '���[�U�h�c���s���ׁ̈A�X�V�ł��܂���';
	errOutPut($mess);
}

if(isset($post) && !empty($post)){
	//post�̒l�����݂��Ă��鎞
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'update'){
		//�t�@�C�����X�V��
		//�t�@�C�����`�F�b�N
		if(preg_match('/[\\\\\/:*?"<>|\']/',$file['name'])){
			array_push($messages, '�t�@�C�����Ɏ��̕����͎g�p�ł��܂���B\/:*?"<>|\'</br>');
			$updateerr="1";
		}
		
		//�t�@�C�����ɖ�肪�Ȃ���
		if($updateerr != "1"){
			if(!rename($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8","cp932"),
					   $oConf->getstrExplorerPath() . mb_convert_encoding($file['name'],"utf8", "cp932" ))){
				//�t�@�C���̃��l�[���Ɏ��s�����Ƃ�
				array_push($messages, '�t�@�C�����̕ύX�Ɏ��s���܂����B</br>');
			}else{
				//�t�@�C���̃��l�[���ɐ��������Ƃ�
				array_push($messages, '�t�@�C�������X�V���܂����B</br>');
				$file['name_befor'] = $file['name'];
			}
		}
	}
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'download'){
		//�_�E�����[�h��
		ini_set('memory_limit', '3000M');
		header('Content-Disposition: attachment; filename="' . $file['name_befor'] . '"');
		header('Content-Type: application/octet-stream');
		header('Content-Transfer-Encoding: binary');
		header('Content-Length: ' . filesize($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932")));
		
		if(!readfile($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"))){
			array_push($messages, '�t�@�C���̃_�E�����[�h�Ɏ��s���܂����B</br>');
		}
		exit;
	}
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'remove'){
		//�t�@�C���폜
		if(!unlink($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"))){
			array_push($messages, '�t�@�C���̍폜�Ɏ��s���܂����B</br>');
		}else{
			//�G���[���b�Z�[�W�󂯓n��
			$tmp = urlencode("�t�@�C�����폜���܂����B</br>");
			if(isset($nextExeName) && !empty($nextExeName)){
				header('Location: ./'.$nextExeName.'?message=' .$tmp);
			}else{
				header('Location: ./'.'DataExchange'.'?message=' .$tmp);
			}
			exit;
		}
	}
}

//�t�@�C���̍X�V���Ԏ擾
if(isset($file['name']) && !empty($file['name'])){
	$file['lastmod'] = date('Y/m/d H:i:s', filemtime($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932")));
	$file['filesize'] = filesize($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"));
}

//view�Ăяo��
require_once('./view/vw_DataExchangeDetail.php');

?>