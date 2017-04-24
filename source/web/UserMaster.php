<?php
session_start();
//�R���t�B�O�t�@�C���ǂݍ���
require_once('./lib/config.php');
// //���O�t�@�C���ǂݍ���
require_once('./lib/log.php');
//Permission���X�g�t�@�C���ǂݍ���
require_once('./lib/clsUserCondition.php');

//�Z�b�V�����m�F
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//�G���[����
	$mess = '���[�U�h�c���s���ׁ̈A�X�V�ł��܂���';
	errOutPut($mess);
}

//�N���X�̃C���X�^���X���i�R���t�B�O�j
$clsConfig = new initConf();
$oLog = new Log('');

//�Z�b�V�����m�F
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//�G���[����
	$mess = '���[�U�h�c���s���ׁ̈A�X�V�ł��܂���';
	errOutPut($mess);
}

//�S���[�U�̃��[�U�ݒ�
$resultArray = array();
//���O�C�����[�U�̃��[�U�ݒ�
$loginUser = array();
//�R���{�{�b�N�X�̑I��l
$postArray = $_POST;
//�R���t�B�O�t�@�C���̃J���[�p�^�[��
$colorArray = $clsConfig->getColorNameList();
//�G���[���b�Z�[�W
$mess  = '';
//SQL��
$sql = '';
$array = array();


//DB�ڑ�
try {
	$dbh = new PDO('mysql:dbname='. DB_NAME .';host=' . DB_HOST . ';port='. DB_PORT, DB_USER, DB_PASS);
//�G���[����
} catch (PDOException $e) {
	//DB�ؒf
	$dbh = NULL;
	$mess = 'DB�ڑ��Ɏ��s���܂��� :'.'mysql:dbname='. DB_NAME .';host=' . DB_HOST . ';port='. DB_PORT.':'.$e->getMessage();
	errOutPut($mess);
}
//SQL��(SELECT)�̏���
$sql = 'SELECT user_id,user_name,permission,pg_id,print_id, ';
$sql .= 'bg_color,font_color,font_size,';
$sql .= 'reverse_bg_color,reverse_font_color ';
$sql .= ',authority ';
$sql .= 'FROM M_USER ORDER BY user_id';
//�N�G���̏���
$sth = $dbh->prepare($sql);
//�N�G���̎��s
$sth->execute();
// //DB�ؒf
// $dbh = NULL;
//���O�C�����[�U�̃��[�U�ݒ�i�[
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	if($_SESSION['user_id'] === $row['user_id']){
		$loginUser = $row;
	}
}
//���O�C�����[�U�̃��[�U�ݒ�擾���s
if(empty($loginUser) ){
	$mess = 'DB���烆�[�U���擾�Ɏ��s���܂���';
	errOutPut($mess);
}

//*********************************************���[�U�ݒ�J�n add koyama  20160330
$clsLoginUser = New clsUserCondition($loginUser);

//*********************************************���[�U�ɑ΂��錠���擾 add koyama 20160330
//pg���̎擾
$pgArray = array();
$sql = 'SELECT pg_id,pg_name,permission ';
$sql .= 'FROM M_PG ';
$sql .= ';';
//�N�G���̏���
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}

//�N�G���̎��s
$sth->execute();

//�e�s��ϐ��Ɋi�[
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	$pgArray[] =$row;
}

$clsLoginUser->setPgList($pgArray);


// $oLog->info(' ^ - ^ '. print_r($postArray,true).__FILE__.':'.__LINE__);
//*********************************************���X�V********** or Ajax **********
//�R���{�{�N�X����l�������Ă��邱�Ƃ��m�F
if(isset($postArray) && count($postArray) > 0){
	//add koyama Ajax�p�̏�����
	$oLog->info('query error !!'. print_r($postArray,true).__FILE__.':'.__LINE__);
	if(array_key_exists('ajax_flg',$postArray) && $postArray['ajax_flg'] == 1){
		//�擾�Ώۃ��[�U�̃��[�U�ݒ�
		$getUser = array();
		// $oLog->info('query error !!'. print_r($postArray,true).__FILE__.':'.__LINE__);
		if (strcmp($loginUser['user_id'], $postArray['user_id']) !== 0) {
			//�G���[�ɂȂ������ŕԂ�
			exit;
		}
		$pgArray = array();
		$sql = 'SELECT user_id,user_name,permission,pg_id,print_id, ';
		$sql .= 'bg_color,font_color,font_size,';
		$sql .= 'reverse_bg_color,reverse_font_color ';
		$sql .= ',authority ';
		$sql .= 'FROM M_USER ';
		$sql .= 'WHERE user_id LIKE :user_id ';
		$sql .= 'ORDER BY user_id';
		$sql .= ';';
		//�N�G���̏���
		$sth = $dbh->prepare($sql);
		$sth->bindParam(':user_id',$postArray['chuser_id']);
		if (!$sth) {
			$oLog->info('query error !!'.__FILE__.':'.__LINE__);
			//�G���[�ɂȂ������ŕԂ�
			exit;
		}
		//�N�G���̎��s
		$sth->execute();
		//�擾�Ώۃ��[�U���̊i�[
		while($row = $sth->fetch(PDO::FETCH_ASSOC)){
			//1�s���������͂�
			$getUser = $row;
		}
		//�擾���s
		if(empty($getUser) ){
			//�G���[�ɂȂ������ŕԂ�
			$oLog->info('not found target user !!'.__FILE__.':'.__LINE__);
			exit;
		}
		$tempArray = $clsLoginUser->splitPermissionString($getUser['permission']);
		$oLog->info('query error !!'. print_r($tempArray,true).__FILE__.':'.__LINE__);
		//static�̂悤�Ɏg�p
		foreach($tempArray as $key => $part){
			$getUser[$key] = $part;
		}

		echo json_encode($getUser);
 		exit;
	}else{
		//add koyama ���O�C�����[�U������������
		//���O�C�����[�U�����m�F
		if($clsLoginUser->authority == 0){
		//**********����ʃ��[�U�m�F**********
			//���[�U�h�c�m�F
			if (strcmp($loginUser['user_id'], $postArray['user_id']) !== 0) {
				$mess = '���[�U�h�c���s���Ȃ̂ōX�V�ł��܂���';
				errOutPut($mess);
			}
			//���[�U���m�F
			if (strcmp($loginUser['user_name'], $postArray['user_name']) !== 0) {
				$mess = '���[�U�����s���Ȃ̂ōX�V�ł��܂���';
				errOutPut($mess);
			}

		}
		//**********����ʃ��[�U�m�F�I��**********
		//**********���S���[�U�m�F**********
		//�w�i�F�m�F
		if(!array_key_exists($postArray['bg_color'],$colorArray)){
			$mess = '�w�i�F���s���Ȃ̂ōX�V�ł��܂���';
			errOutPut($mess);
		}
		//�����F�m�F
		if(!array_key_exists($postArray['font_color'],$colorArray)){
			$mess = '�����F���s���Ȃ̂ōX�V�ł��܂���';
			errOutPut($mess);
		}
		//�����T�C�Y�m�F
		if($postArray['font_size'] < MIN_F_SIZE or $postArray['font_size'] > MAX_F_SIZE){
			$mess = '�����T�C�Y���s���Ȃ̂ōX�V�ł��܂���';
			errOutPut($mess);
		}
		//�����w�i�F�m�F
		if(!array_key_exists($postArray['reverse_bg_color'],$colorArray)){
			$mess = '�����w�i�F���s���Ȃ̂ōX�V�ł��܂���';
			errOutPut($mess);
		}
		//���������F�m�F
		if(!array_key_exists($postArray['reverse_font_color'],$colorArray)){
			$mess = '���������F���s���Ȃ̂ōX�V�ł��܂���';
			errOutPut($mess);
		}
		//**********���S���[�U�m�F�I��**********
		//UPDATE�p�̒l�̍쐬
		$strPermission = $clsLoginUser->concatPermissionString($postArray);

		//SQL��(UPDATE)�̏���
		$sql = 'UPDATE M_USER SET ';
		$sql .= 'bg_color = :bg_color,';
		$sql .= 'font_color = :font_color,';
		$sql .= 'font_size = :font_size,';
		$sql .= 'reverse_bg_color = :reverse_bg_color,';
		$sql .= 'reverse_font_color = :reverse_font_color, ';
		$sql .= 'print_id = :printer_id ';
		if($clsLoginUser->authority != 0){
			$sql .= ',pg_id = :pg_id ';
			$sql .= ',permission = :permission ';
		}
		$sql .= 'WHERE user_id = :user_id';
		//�N�G���̏���
		$sth = $dbh->prepare($sql);
		if (!$sth) {
			$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
		}
		$sth->bindParam(':user_id',$postArray['user_id']);
		$sth->bindParam(':bg_color',$postArray['bg_color']);
		$sth->bindParam(':font_color',$postArray['font_color']);
		$sth->bindParam(':font_size',$postArray['font_size']);
		$sth->bindParam(':reverse_bg_color',$postArray['reverse_bg_color']);
		$sth->bindParam(':reverse_font_color',$postArray['reverse_font_color']);
		$sth->bindParam(':printer_id',$postArray['printer_id']);
		if($clsLoginUser->authority != 0){
			$sth->bindParam(':pg_id',$postArray['pg_id']);
			$sth->bindParam(':permission',$strPermission);
		}
		//�N�G���̎��s
		$sth->execute();
		$oLog->info('query !!' . $sql.' : '. print_r($postArray,true).__FILE__.':'.__LINE__);
	}
}
//**********���X�V�I��**********
$oLog->info(' ^ - < '.__FILE__.':'.__LINE__);
$sql = 'SELECT user_id,user_name,permission,pg_id,print_id, ';
$sql .= 'bg_color,font_color,font_size,';
$sql .= 'reverse_bg_color,reverse_font_color ';
$sql .= ',authority ';
$sql .= 'FROM M_USER ORDER BY user_id';
//�N�G���̏���
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}
//�N�G���̎��s
$sth->execute();


//���O�C�����[�U�̃��[�U�ݒ�i�[
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	array_push($resultArray,$row);
	if($_SESSION['user_id'] === $row['user_id']){
		$loginUser = $row;
	}
}
//���O�C�����[�U�̃��[�U�ݒ�擾���s
if(empty($loginUser) ){
	$mess = 'DB���烆�[�U���擾�Ɏ��s���܂���';
	errOutPut($mess);
}
//**********�S���[�U�ݒ�擾�I��**********

//********************************************�X�V���ꂽ�l������\�������邽�ߓ���Ȃ���
$clsLoginUser->resetProp($loginUser);

//*********************************************�v�����^�̃��X�g�擾 add koyama 20160401
//printer���̎擾
$printArray = array();
$sql = 'SELECT print_id,print_name ';
$sql .= 'FROM M_PRINTER ';
$sql .= ';';
//�N�G���̏���
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}
//�N�G���̎��s
$sth->execute();

//�e�s��ϐ��Ɋi�[
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
// 	shell_exec("logger -i ' -- ".print_r($row,true)." -- '");
	$printArray[] = $row;
}

$clsLoginUser->setPrinterList($printArray);

header('Expires: Thu, 01 Dec 1994 16:00:00 GMT');
header('Last-Modified: ' . gmdate('D, d M Y H:i:s') . ' GMT');
header('Cache-Control: no-store, no-cache, must-revalidate');
header('Cache-Control: post-check=0, pre-check=0', false);
header('Pragma: no-cache');
require_once('./view/vw_userMaster.php');
