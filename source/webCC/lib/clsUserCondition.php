<?php
//���[�U�\���̍ۂ̃��j���[�Ƃ̑Ή����i�[
//author koyama
//create 20160330
class clsUserCondition{
	public $oLog;
	public $userID;
	public $userName;
	public $firstpg;			//M_USER�̏����\�����j���[��ID
	public $permission;			//M_USER��permission������
	public $permissionArray;	//���j���[�Ƃ��̎g�p�s���i�[����N���X��Array
	public $printId;
	public $bgColor;
	public $fontColor;
	public $fontSize;
	public $reverseBgColor;
	public $reverseFontColor;
	public $printer;			//printer_id���i�[(�O���̔�r�Ɏg��)
	public $printerArray;			//printer_id���i�[(�O���̔�r�Ɏg��)
	public $authority;			//authority (0:��� 1:�Ǘ���)

	function __construct($userSetting){
		require_once('./lib/log.php');
		$this->oLog = New Log('');
		$this->userID           = $userSetting['user_id'];
		$this->userName         = $userSetting['user_name'];
		$this->firstpg          = $userSetting['pg_id'];
		$this->permission       = $userSetting['permission'];
		$this->printId          = $userSetting['print_id'];
		$this->bgColor          = $userSetting['bg_color'];
		$this->fontColor        = $userSetting['font_color'];
		$this->fontSize         = $userSetting['font_size'];
		$this->reverseBgColor   = $userSetting['reverse_bg_color'];
		$this->reverseFontColor = $userSetting['reverse_font_color'];
		$this->authority        = $userSetting['authority'];
		$this->permissionArray  = array();
		$this->printerArray     = array();
// 		$this->oLog->info('^^'.print_r($this,true));
	}

	function resetProp($userSetting){
		$this->userName         = $userSetting['user_name'];
		$this->permission       = $userSetting['permission'];
		$this->firstpg          = $userSetting['pg_id'];
		$this->printId          = $userSetting['print_id'];
		$this->bgColor          = $userSetting['bg_color'];
		$this->fontColor        = $userSetting['font_color'];
		$this->fontSize         = $userSetting['font_size'];
		$this->reverseBgColor   = $userSetting['reverse_bg_color'];
		$this->reverseFontColor = $userSetting['reverse_font_color'];
// 		$this->oLog->info('^^'.print_r($this,true));
	}

	//pglist
	function setPgList($listFromDB){
// 	shell_exec("logger -i ' -o^ ".print_r($listFromDB,true)." o- '");
		foreach($listFromDB as $row){
			$items = New clsPgPermission($row);
			array_push($this->permissionArray,$items);
		}
	}

	//printer
	function setPrinterList($listFromDB){
		foreach($listFromDB as $row){
			$items = New clsPrinterItem($row);
			array_push($this->printerArray,$items);
		}
	}

	//permission�̕����������ƃv���p�e�B���[�U��permission�Ɣ�r����
	//author:koyama
	//Return:����\ = 1,����s�\ = 0
	function checkPermission($strPermission){
// 		shell_exec("logger -i ' --1-- ".print_r($strPermission,true)." o- '");
		if(substr($this->permission,strlen($strPermission) - 1,1) == '1'){
			return 1;
		}
		return 0;
	}

	//
	//permission��post�f�[�^�𕶎���ɖ߂�����
	//author:koyama
	//Return:0 or 1�̘A�����錠��������
	function concatPermissionString($postArray){
		$retVal = ' ';
		//�\�߃X�y�[�X�����̒�������Ă����Ďn�߂�
		$retVal = str_repeat($retVal,count($this->permissionArray));

		foreach($this->permissionArray as $oPermission){
			//�������\�����Ă���̂ŏ������Ō���
			if(array_key_exists(strtolower($oPermission->name),$postArray)){
				$retVal = substr_replace($retVal,'1',($oPermission->id - 1),1);
			}
		}
		$retVal = preg_replace('/ /','0',$retVal);
// 		shell_exec("logger -i ' --2-- :".$retVal.": o- '");
		return $retVal;
	}

	//permission�̕�����f�[�^��on/off�̔z��ɖ߂�����
	//author:koyama
	//Return:���j���[���ɑ΂���1/0�̔z��
	function splitPermissionString($inputVal){
		$retVal = array();
		
		if(strlen($inputVal) != count($this->permissionArray)){
			//���g�����܂����Ȃ��ꍇ��ŕԂ�
			return $retVal;
		}
		foreach($this->permissionArray as $oPermission){
			//�������\�����Ă���̂ŏ������Ō���
			$retVal[ 'pem' . strtolower($oPermission->name)] = substr($inputVal,($oPermission->id - 1),1);
		}
		$retVal = preg_replace('/ /','0',$retVal);
		return $retVal;
	}
}

//���[�U�\���̍ۂ̃��j���[�ɑ΂���\���s��
//author koyama
//create 20160330
class clsPgPermission{
	public $id;
	public $name;
	public $permission;

	function __construct($var){
		$this->id       =$var['pg_id'];
		$this->name     =$var['pg_name'];
		$this->permission =$var['permission'];
	}
}
//���[�U�\���̍ۂ̃��j���[�ɑ΂���v�����^
//author koyama
//create 20160401
class clsPrinterItem{
	public $id;
	public $name;

	function __construct($var){
		$this->id       =$var['print_id'];
		$this->name     =$var['print_name'];
	}
}
?>
