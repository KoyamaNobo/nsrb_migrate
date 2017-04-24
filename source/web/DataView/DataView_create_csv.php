<?php
////////////////////////////////////////////////////////////////////////////////
//���ʃ��C�u�����ǂݍ���
////////////////////////////////////////////////////////////////////////////////
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//���s���Ԗ�����
////////////////////////////////////////////////////////////////////////////////
set_time_limit(0);

////////////////////////////////////////////////////////////////////////////////
//�ϐ�������
////////////////////////////////////////////////////////////////////////////////
$sql         = '';
$sqlExec     = '';
if(isset($argv[1])){
	$sql         = $argv[1];
	//�u�V���O���N�H�[�g���u##�v�œn���Ă���̂ŋt�ϊ�
	$sql = str_replace("##", "'", $sql);
}
$sqlexe       = '';
$limit_start  = 0;
$contents     = '';
$disps        = array();
$breakFlag    = false;
$message_codes = '';

////////////////////////////////////////////////////////////////////////////////
//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	echo $message_codes."\r\n";
	return false;
}

//SQL������LIMIT�����O�������̂��擾�i�����́u;�v���O�������́j
$sqlExec = getSqlExec($sql);

//���sSQL����ʕ\���p�̍��ږ��̕������擾
$disps = getSqlDisps($sqlExec);


while ($breakFlag == false ) {
	//SQL���쐬
	$sqlexe  = $sqlExec; //���񏉊����iLIMIT��ς��邽�߁j
	$sqlexe .= ' LIMIT '.$limit_start.' , '.CSVNUM.' ; ';
	$sth = $dbhNIS->prepare($sqlexe);
	//SQL���s
	$sth->execute();
	
	$dataCount = 0;
	while($data = $sth->fetch(PDO::FETCH_ASSOC)){
		$loopCount = 0;
		$contents = '';
		foreach($disps as $disp){
			if($loopCount != 0 ){
				$contents .= ',';
			}
			$contents .= $data[$disp["dispname"]];
			$loopCount++;
		}
		
		//CSV�ɏ�������
		echo $contents . PHP_EOL;
		$dataCount++;
	}
	
	if($dataCount == 0){
		//�f�[�^�����݂��Ȃ��Ƃ������𔲂���
		$breakFlag = true;
	}
	
	//����SQL���s�p��LIMIT�l��ς���
    $limit_start = $limit_start + CSVNUM;
}

?>