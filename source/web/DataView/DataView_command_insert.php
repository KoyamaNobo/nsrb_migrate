<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post           = $_POST;
$today          = date("Y-m-d H:i:s");
$Cre_Date       = $today;
$Mod_Date       = $today;
$Del_Flg        = '0';
$message_codes  = '';

//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	echo $message_codes."\r\n";
	return false;
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	echo $message_codes."\r\n";
	return false;
}

if(empty($post) || empty($post['Command_Name']) ){
	//post�p�����[�^�����݂��Ȃ��Ƃ�
	echo "ERR0701\r\n";
	put_error('ERR0701',$e);
	return false;
}

//SQL���쐬�i�C���T�[�g���j
$sql  = " INSERT INTO t_command (Command_Id,Command_Name,User_Id,Sql_str,Cre_Date,Mod_Date,Del_Flg ) ";
$sql .= " VALUES( DEFAULT,:Command_Name,:User_Id,:Sql_str,:Cre_Date,:Mod_Date,:Del_Flg) ;";

//SQL�����s�i�C���T�[�g���j
$sth = $dbhDV->prepare($sql);
$tmp = mb_convert_encoding($post['Command_Name'], "SJIS", "UTF-8");
$sth->bindParam('Command_Name',$tmp);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
$sth->bindParam('Sql_str',$post['Sql_str']);
$sth->bindParam('Cre_Date',$Cre_Date);
$sth->bindParam('Mod_Date',$Mod_Date);
$sth->bindParam('Del_Flg',$Del_Flg);
//SQL���s
$sth->execute();

//SQL�쐬�i�C���T�[�g�m�F�j
$sql  = " SELECT  *  ";
$sql .= " FROM t_command  ";
$sql .= " WHERE Command_Name = :Command_Name ";
$sql .= " AND User_Id = :User_Id ";
$sql .= " AND Sql_str = :Sql_str ";
$sql .= " AND Del_Flg = :Del_Flg ";
//SQL�����s�i�C���T�[�g�m�F�j
$sth = $dbhDV->prepare($sql);
$tmp = mb_convert_encoding($post['Command_Name'], "SJIS", "UTF-8");
$sth->bindParam('Command_Name',$tmp);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
$sth->bindParam('Sql_str',$post['Sql_str']);
$sth->bindParam('Del_Flg',$Del_Flg);
//SQL���s
$sth->execute();

//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
$count = 0;
while($command = $sth->fetch(PDO::FETCH_ASSOC)){
	$count++;
}
if($count > 0){
	//�C���T�[�g������
	echo "INF0001\r\n";
}else{
	//�C���T�[�g���s��
	echo "ERR0016\r\n";
	put_error('ERR0016','');
}

return;
?>
