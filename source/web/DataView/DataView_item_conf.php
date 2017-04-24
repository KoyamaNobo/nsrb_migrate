<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post          = $_POST;
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
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//�C���T�[�gor�A�b�v�f�[�g����
if(!empty($post)){
	$TableName     = $post['TableName'];
	$ItemName      = $post['ItemName'];
	$Japanese_Name = $post['JapaneseName'];
	$NonDisp_Flg   = $post['NonDisp_Flg'];
	$S_point       = $post['S_point'];
	$Size          = $post['Size'];
	$Type          = $post['Type'];

	$sql  = " SELECT * FROM m_japanese_name WHERE TableName=:TableName AND Label=:Label AND S_Point=:S_Point AND Size=:Size AND DataType=:DataType ";
	$sth = $dbhDV->prepare($sql);
	$sth->bindParam('TableName',$TableName);
	$sth->bindParam('Label',$ItemName);
	$sth->bindParam('S_Point',$S_point);
	$sth->bindParam('Size',$Size);
	$sth->bindParam('DataType',$Type);
	$sth->execute();

 	$loopCount = 0;
 	while($item = $sth->fetch(PDO::FETCH_ASSOC)){
 		$loopCount++;
 	}

	if($loopCount != 0){
		//���݂��Ă���Ƃ��̓A�b�v�f�[�g
		$sql  = " UPDATE m_japanese_name SET Japanese_Name=:Japanese_Name , NonDisp_Flg=:NonDisp_Flg , Mod_Date=now() ";
		$sql .= " WHERE TableName=:TableName AND Label=:Label AND S_Point=:S_Point AND Size=:Size AND DataType=:DataType ";

		$sth = $dbhDV->prepare($sql);

		$sth->bindParam('Japanese_Name',$Japanese_Name);
		$sth->bindParam('NonDisp_Flg',$NonDisp_Flg);
		$sth->bindParam('TableName',$TableName);
		$sth->bindParam('Label',$ItemName);
		$sth->bindParam('S_Point',$S_point);
		$sth->bindParam('Size',$Size);
		$sth->bindParam('DataType',$Type);

		$sth->execute();

		//todo�A�b�v�f�[�g�m�F��t����
		$sql  = " SELECT count(Label) as num FROM m_japanese_name ";
		$sql .= " WHERE TableName=:TableName AND Label=:Label AND S_Point=:S_Point AND Size=:Size AND DataType=:DataType AND Japanese_Name=:Japanese_Name AND NonDisp_Flg=:NonDisp_Flg ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Japanese_Name',$Japanese_Name);
		$sth->bindParam('NonDisp_Flg',$NonDisp_Flg);
		$sth->bindParam('TableName',$TableName);
		$sth->bindParam('Label',$ItemName);
		$sth->bindParam('S_Point',$S_point);
		$sth->bindParam('Size',$Size);
		$sth->bindParam('DataType',$Type);

		$sth->execute();
		//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
		$count = 0;
		while($command = $sth->fetch(PDO::FETCH_ASSOC)){
			$count = $command['num'];
		}
		if($count > 0){
			//�C���T�[�g������
			$successCode   = 'INF0005';
		}else{
			//�C���T�[�g���s��
			$message_codes = 'ERR0017';
			put_error($message_codes,'');
		}

	}else{
		//���݂��Ă��Ȃ��Ƃ��̓C���T�[�g
		$sql  = " INSERT INTO m_japanese_name (TableName,Label,S_Point,Size,DataType,Japanese_Name,NonDisp_Flg,Cre_Date,Mod_Date,Del_Flg ) ";
		$sql .= " VALUES( :TableName,:Label,:S_Point,:Size,:DataType,:Japanese_Name,:NonDisp_Flg,:Cre_Date,now(),:Del_Flg ) ;";
		$sth = $dbhDV->prepare($sql);

		$sth->bindParam('TableName',$TableName);
		$sth->bindParam('Label',$ItemName);
		$sth->bindParam('S_Point',$S_point);
		$sth->bindParam('Size',$Size);
		$sth->bindParam('DataType',$Type);
		$sth->bindParam('Japanese_Name',$Japanese_Name);
		$sth->bindParam('NonDisp_Flg',$NonDisp_Flg);
		$sth->bindParam('Cre_Date',$Cre_Date);
		$sth->bindParam('Del_Flg',$Del_Flg);

		$sth->execute();

		//�C���T�[�g�m�F
		$sql  = " SELECT * FROM m_japanese_name ";
		$sql .= " WHERE TableName=:TableName AND Label=:Label AND S_Point=:S_Point AND Size=:Size AND DataType=:DataType AND Japanese_Name=:Japanese_Name AND NonDisp_Flg=:NonDisp_Flg AND Del_Flg=:Del_Flg ;";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Japanese_Name',$Japanese_Name);
		$sth->bindParam('NonDisp_Flg',$NonDisp_Flg);
		$sth->bindParam('TableName',$TableName);
		$sth->bindParam('Label',$ItemName);
		$sth->bindParam('S_Point',$S_point);
		$sth->bindParam('Size',$Size);
		$sth->bindParam('DataType',$Type);
		$sth->bindParam('Del_Flg',$Del_Flg);

		$sth->execute();

		//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
		$count = 0;
		while($command = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}
		if($count > 0){
			//�C���T�[�g������
			$successCode   = 'INF0005';
		}else{
			//�C���T�[�g���s��
			$message_codes = 'ERR0018';
			put_error($message_codes,'');
		}

	}
}
//���ڈꗗ�擾
//upd koyama ��Œǉ���Ajax�Ŏ擾����
get_items($items,$message_codes,$dbhNIS,0);

//��ʕ`��d
require_once('./view/vw_DataView_Item_Conf.php');

?>
