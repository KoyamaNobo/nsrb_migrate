<?php
//ユーザ表示の際のメニューとの対応を格納
//author koyama
//create 20160330
class clsUserCondition{
	public $oLog;
	public $userID;
	public $userName;
	public $firstpg;			//M_USERの初期表示メニューのID
	public $permission;			//M_USERのpermission文字列
	public $permissionArray;	//メニューとその使用可不可を格納するクラスのArray
	public $printId;
	public $bgColor;
	public $fontColor;
	public $fontSize;
	public $reverseBgColor;
	public $reverseFontColor;
	public $printer;			//printer_idを格納(外側の比較に使う)
	public $printerArray;			//printer_idを格納(外側の比較に使う)
	public $authority;			//authority (0:一般 1:管理者)

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

	//permissionの文字列を入れるとプロパティユーザのpermissionと比較する
	//author:koyama
	//Return:操作可能 = 1,操作不能 = 0
	function checkPermission($strPermission){
// 		shell_exec("logger -i ' --1-- ".print_r($strPermission,true)." o- '");
		if(substr($this->permission,strlen($strPermission) - 1,1) == '1'){
			return 1;
		}
		return 0;
	}

	//
	//permissionのpostデータを文字列に戻す処理
	//author:koyama
	//Return:0 or 1の連続する権限文字列
	function concatPermissionString($postArray){
		$retVal = ' ';
		//予めスペースをその長さ入れておいて始める
		$retVal = str_repeat($retVal,count($this->permissionArray));

		foreach($this->permissionArray as $oPermission){
			//小文字表示しているので小文字で検索
			if(array_key_exists(strtolower($oPermission->name),$postArray)){
				$retVal = substr_replace($retVal,'1',($oPermission->id - 1),1);
			}
		}
		$retVal = preg_replace('/ /','0',$retVal);
// 		shell_exec("logger -i ' --2-- :".$retVal.": o- '");
		return $retVal;
	}

	//permissionの文字列データをon/offの配列に戻す処理
	//author:koyama
	//Return:メニュー名に対して1/0の配列
	function splitPermissionString($inputVal){
		$retVal = array();
		
		if(strlen($inputVal) != count($this->permissionArray)){
			//中身がうまく作れない場合空で返す
			return $retVal;
		}
		foreach($this->permissionArray as $oPermission){
			//小文字表示しているので小文字で検索
			$retVal[ 'pem' . strtolower($oPermission->name)] = substr($inputVal,($oPermission->id - 1),1);
		}
		$retVal = preg_replace('/ /','0',$retVal);
		return $retVal;
	}
}

//ユーザ表示の際のメニューに対する表示可不可
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
//ユーザ表示の際のメニューに対するプリンタ
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
