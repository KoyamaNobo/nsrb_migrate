	<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$commands = array();                          //�擾���������^�O�ꗗ���i�[
$Del_Flg       = '0';                         //�폜�t���O���i�[
$today         = date("Y-m-d H:i:s");         //�T�[�o�̓��t�擾
$Mod_Date      = $today;                      //�ύX�����i�[
$post          = $_POST;                      //post�l���i�[
$successCode   = '';                          //�������b�Z�[�W�R�[�h���i�[
$message_codes = '';                          //�G���[���b�Z�[�W�R�[�h���i�[

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
	//POST�����݂��Ă��鎞
	if(!empty($post['action']) && $post['action']=="del" ){

		//�폜�̂Ƃ�
		$Command_Id = $post['Command_Id'];

		//SQL�쐬�i�A�b�v�f�[�g�j
		$sql = " UPDATE t_command SET Del_Flg='1',Mod_Date=:Mod_Date WHERE Command_Id=:Command_Id AND User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Command_Id',$Command_Id);
		$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
		$sth->bindParam('Mod_Date',$Mod_Date);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();

		//SQL�쐬�i�A�b�v�f�[�g�m�F�j
		$sql = " SELECT Command_Id,User_Id,Del_Flg FROM t_command WHERE Command_Id=:Command_Id AND User_Id=:User_Id AND Del_Flg='1' ;";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Command_Id',$Command_Id);
		$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
		//SQL���s
		$sth->execute();
		$loopCount = 0;
		while($command = $sth->fetch(PDO::FETCH_ASSOC)){
			$loopCount++;
		}
		if($loopCount!=0){
			$successCode   = 'INF0006';
		}else{
			//�A�b�v�f�[�g�m�F�Ń��R�[�h���擾�ł��Ȃ������ꍇ
			addCode($message_codes,'ERR0015');
			put_error('ERR0015',$e);
		}
	}
}

//�����^�O�ꗗ�擾
get_commands($commands,$message_codes,$dbhDV);

//��ʕ`��
require_once('./view/vw_DataView_command_search.php');

?>
