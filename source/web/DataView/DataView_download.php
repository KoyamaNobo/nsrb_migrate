<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//���s���Ԗ�����
////////////////////////////////////////////////////////////////////////////////
//set_time_limit(0);

//POST�̈��������݂��Ă��邩
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['Output_file']) && !empty($_POST['Output_file']) ){
		$post        = $_POST;
		$outputfile  = $post['Output_file']; //�_�E�����[�h�̃t�@�C����
	}
}else{
		put_error('ERR0501','');
		exit();
}

//�_�E�����[�h��
ini_set('memory_limit', '3000M');
header('Content-Disposition: attachment; filename="' . $outputfile . '"');
header('Content-Type: application/octet-stream');
header('Content-Transfer-Encoding: binary');
header('Content-Length: ' . filesize(CSV_PATH . $outputfile));

if(!readfile(CSV_PATH . $outputfile)){
    put_error('ERR0019','');
}

?>