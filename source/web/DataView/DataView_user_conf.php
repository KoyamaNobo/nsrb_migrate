<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$post          = $_POST;
$today         = date("Y-m-d H:i:s");
$userAuthority = '';                            //���O�C�����Ă��郆�[�U�̌���
$User_Id       = '';
$User_Password = '';
$User_Conf     = '';
$Authority_Flg = '';
$Disp_Num      = '';
$Cre_Date      = $today;
$Mod_Date      = $today;
$Del_Flg       = '';
$successCode   = '';
$message_codes = '';
$users         = array();
$url           = basename($_SERVER['PHP_SELF'],".php");


//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}


//SQL���쐬�i�A�b�v�f�[�g�m�F�p�j
$sql  = " SELECT Authority_Flg ";
$sql .= " FROM m_user ";
$sql .= " WHERE ";
$sql .= "     User_Id=:User_Id ; ";
$sth = $dbhDV->prepare($sql);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
//SQL���s
$sth->execute();

//�P�����������͂�
$count = 0;
while($user = $sth->fetch(PDO::FETCH_ASSOC)){
	$userAuthority = $user['Authority_Flg'];
}


//�o�^or�폜or�X�Vor�X�V��ʌĂяo������
if(!empty($post)){
	if(!empty($post['action']) && $post['action']=="add" ){

		//POST�f�[�^�擾
		$User_Id       = $post['User_Id'];
		$User_Password = $post['User_Password'];
		$User_Conf     = $post['User_Conf'];
		shell_exec('logger -i "authority '.$post['Authority_Flg'].'"');
		$Authority_Flg = $post['userAuthority'];
		$Disp_Num      = $post['Disp_Num'];
		$Del_Flg       = '0';

		//SQL���쐬�i�C���T�[�g���j
		$sql  = " INSERT INTO m_user (User_Id,User_Password,Authority_Flg,Disp_Num,Cre_Date,Mod_Date,Del_Flg ) ";
		$sql .= " VALUES( :User_Id,:User_Password,:Authority_Flg,:Disp_Num,now(),now(),:Del_Flg) ;";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('User_Password',$User_Password);
		$sth->bindParam('Authority_Flg',$Authority_Flg);
		$sth->bindParam('Disp_Num',$Disp_Num);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();

		//SQL���쐬�i�C���T�[�g�m�F�p�j
		$sql  = " SELECT * ";
		$sql .= " FROM m_user ";
		$sql .= " WHERE ";
		$sql .= "     User_Id=:User_Id AND ";
		$sql .= "     User_Password=:User_Password AND ";
		$sql .= "     Authority_Flg=:Authority_Flg AND ";
		$sql .= "     Disp_Num=:Disp_Num AND ";
		$sql .= "     Del_Flg=:Del_Flg ; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('User_Password',$User_Password);
		$sth->bindParam('Authority_Flg',$Authority_Flg);
		$sth->bindParam('Disp_Num',$Disp_Num);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();

		//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}

		if($count > 0){
			//�C���T�[�g������
			//��ʕ`��p�ϐ�������
			$User_Id       = '';
			$User_Password = '';
			$Authority_Flg = '';
			$Disp_Num      = '';
			$successCode   = 'INF0002';
		}else{
			//�C���T�[�g���s��
			addCode($message_codes,'ERR0010');
			put_error('ERR0010','');
		}

	}else if(!empty($post['action']) && $post['action']=="del" ){

		//POST�f�[�^�擾
		$User_Id       = $post['User_Id'];
		$Del_Flg       = '0';

		//SQL���쐬�i�폜�O�m�F�p�j
		$sql = " SELECT User_Id,Del_Flg FROM m_user WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();
		//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}
		if($count == 0){
			//���O�m�F���s��
			addCode($message_codes,'ERR0011');
			put_error('ERR0011','');
		}

		if($message_codes == ''){
			//SQL���쐬�i�폜�p�j
			$sql = " UPDATE m_user SET Del_Flg='1',Mod_Date=:Mod_Date WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
			$sth = $dbhDV->prepare($sql);
			$sth->bindParam('User_Id',$User_Id);
			$sth->bindParam('Mod_Date',$Mod_Date);
			$sth->bindParam('Del_Flg',$Del_Flg);
			//SQL���s
			$sth->execute();

			//SQL���쐬�i�폜�m�F�p�j
			$sql  = " SELECT * ";
			$sql .= " FROM m_user ";
			$sql .= " WHERE ";
			$sql .= "     User_Id=:User_Id AND ";
			$sql .= "     Mod_Date=:Mod_Date AND ";
			$sql .= "     Del_Flg='1' ; ";
			$sth = $dbhDV->prepare($sql);
			$sth->bindParam('User_Id',$User_Id);
			$sth->bindParam('Mod_Date',$Mod_Date);
			//SQL���s
			$sth->execute();
			//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
			$count = 0;
			while($user = $sth->fetch(PDO::FETCH_ASSOC)){
				$count++;
			}
			if($count > 0){
				//����I�������獶�̒l���폜����
				$User_Id       = '';
				$successCode   = 'INF0003';
			}else{
				//�폜���s��
				addCode($message_codes,'ERR0012');
				put_error('ERR0012','');
			}
		}
	}else if(!empty($post['action']) && $post['action']=="detail" ){

		//POST�f�[�^�擾
		$User_Id       = $post['User_Id'];
		$Del_Flg       = '0';

		//SQL�쐬�i���[�U�擾�p�j
		$sql = " SELECT User_Id,User_Password,Authority_Flg,Disp_Num,Del_Flg FROM m_user WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();

		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$User_Id       = $user['User_Id'];
			$User_Password = $user['User_Password'];
			$Authority_Flg = $user['Authority_Flg'];
			$Disp_Num      = $user['Disp_Num'];
			$count++;
		}

		if($count > 0){
			//���[�U�X�V��ʂ�\�����ďI��
			require_once('./view/vw_DataView_User_Conf_Mod.php');
			exit;
		}else{
			//�폜���s��
			addCode($message_codes,'ERR0014');
			put_error('ERR0014','');
		}
	}else if(!empty($post['action']) && $post['action']=="mod" ){
		//�X�V��
		//POST�f�[�^�擾
		$User_Id       = $post['User_Id'];
		$User_Password = $post['User_Password'];
		$User_Conf     = $post['User_Conf'];
		$Authority_Flg = $post['Authority_Flg'];
		$Disp_Num      = $post['Disp_Num'];
		$Del_Flg       = '0';

		shell_exec('logger -i "^^^Authority_Flg'.$Authority_Flg.'"');

		//SQL���쐬�i�A�b�v�f�[�g���j
		$sql  = " UPDATE m_user SET ";
		if($User_Password != ''){
			$sql .= ' User_Password=:User_Password ,';
		}
		$sql .= " Authority_Flg=:Authority_Flg , Disp_Num=:Disp_Num ,Mod_Date=now() ";
		$sql .= " ,Del_Flg=:Del_Flg ";
		$sql .= " WHERE User_Id=:User_Id ; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		if($User_Password != ''){
			$sth->bindParam('User_Password',$User_Password);
		}
		$sth->bindParam('Authority_Flg',$Authority_Flg);
		$sth->bindParam('Disp_Num',$Disp_Num);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		shell_exec('logger -i "^^^$sql'.$sql.'"');
		shell_exec('logger -i "^^^Del_Flg'.$Del_Flg.'"');
		$sth->execute();

		//SQL���쐬�i�A�b�v�f�[�g�m�F�p�j
		$sql  = " SELECT * ";
		$sql .= " FROM m_user ";
		$sql .= " WHERE ";
		$sql .= "     User_Id=:User_Id AND ";
		if($User_Password != ''){
			$sql .= "     User_Password=:User_Password AND ";
		}
		$sql .= "     Authority_Flg=:Authority_Flg AND ";
		$sql .= "     Disp_Num=:Disp_Num AND ";
		$sql .= "     Del_Flg=:Del_Flg ; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		if($User_Password != ''){
			$sth->bindParam('User_Password',$User_Password);
		}
		$sth->bindParam('Authority_Flg',$Authority_Flg);
		$sth->bindParam('Disp_Num',$Disp_Num);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL���s
		$sth->execute();


		//�P���ȏ㑶�݂��Ă��邱�Ƃ��m�F
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}
		if($count > 0){
			if($_SESSION['dv_user_id'] == $User_Id){
				//���O�C��ID�Ɠ���̃��[�UID���X�V�����ꍇ�Z�b�V�����������ׂď���������
				$_SESSION['dv_user_id']            = $User_Id;
				if($User_Password != ''){
					$_SESSION['dv_user_password']      = $User_Password;
				}
				$_SESSION['dv_user_authority_flg'] = $Authority_Flg;
				$_SESSION['dv_user_disp_num']      = $Disp_Num;
			}
			//����I�������獶�̒l���폜����
			$User_Id       = '';
			$User_Password = '';
			$Authority_Flg = '';
			$Disp_Num      = '';
			//�o�͗p���b�Z�[�W�R�[�h
			$successCode   = 'INF0004';
		}else{
			//�폜���s��
			addCode($message_codes,'ERR0013');
			put_error('ERR0013',$e);
		}
	}
}

//���[�U�[�ꗗ�擾
get_users($users,$message_codes,'0',$dbhDV);

$i=0;
//��ʂɕ\�����邽�ߌ����ϊ�
foreach($users as $user){
	if($user['Authority_Flg'] == '1'){
		$users[$i]["A_Flg_Disp"] = '�L��';
	}else{
		$users[$i]["A_Flg_Disp"] = '����';
	}
	$i++;
}

//��ʕ`��
require_once('./view/vw_DataView_User_Conf.php');

?>
