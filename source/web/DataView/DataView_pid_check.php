<?php

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//POST�̈��������݂��Ă��邩
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['PID']) && !empty($_POST['PID']) ){
		$post        = $_POST;
		$pid         = $post['PID'];
		$outputfile  = CSV_PATH .$post['Output_file']; //���R�[�h����
	}else{
	    echo('ERR0501');
        put_error('ERR0501','');
        exit();
	}
}

$ret = "false";
//�v���Z�XID�͌J��Ԃ��g�p����邽��
//�C���v�b�g�t�@�C���ƃA�E�g�v�b�g�t�@�C�����m�F�����^�O�Ɋ܂߂�
$cmd = "/bin/ps -p " .$pid. " -o pid,stat,start,time,command ";


$fp  = popen($cmd, "r");
while( ($line = fgets($fp)) != false ){
	if( intval(trim($line)) == $pid ){
		$ret = "true";
		break;
	}
}

$ret = "PIDinfo:".$ret;

//�������݊��������𒲂ׂ�
$count_str = exec( 'wc -l '.$outputfile );
$count = explode(" ", $count_str);

$ret .= ",WritingCount:".$count[0];


//PID�̏�ԂƏ������݊����������o�͂���
echo $ret;

?>