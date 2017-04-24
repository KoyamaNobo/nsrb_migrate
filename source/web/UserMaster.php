<?php
session_start();
//コンフィグファイル読み込み
require_once('./lib/config.php');
// //ログファイル読み込み
require_once('./lib/log.php');
//Permissionリストファイル読み込み
require_once('./lib/clsUserCondition.php');

//セッション確認
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//エラー処理
	$mess = 'ユーザＩＤが不正の為、更新できません';
	errOutPut($mess);
}

//クラスのインスタンス化（コンフィグ）
$clsConfig = new initConf();
$oLog = new Log('');

//セッション確認
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//エラー処理
	$mess = 'ユーザＩＤが不正の為、更新できません';
	errOutPut($mess);
}

//全ユーザのユーザ設定
$resultArray = array();
//ログインユーザのユーザ設定
$loginUser = array();
//コンボボックスの選択値
$postArray = $_POST;
//コンフィグファイルのカラーパターン
$colorArray = $clsConfig->getColorNameList();
//エラーメッセージ
$mess  = '';
//SQL文
$sql = '';
$array = array();


//DB接続
try {
	$dbh = new PDO('mysql:dbname='. DB_NAME .';host=' . DB_HOST . ';port='. DB_PORT, DB_USER, DB_PASS);
//エラー処理
} catch (PDOException $e) {
	//DB切断
	$dbh = NULL;
	$mess = 'DB接続に失敗しました :'.'mysql:dbname='. DB_NAME .';host=' . DB_HOST . ';port='. DB_PORT.':'.$e->getMessage();
	errOutPut($mess);
}
//SQL文(SELECT)の準備
$sql = 'SELECT user_id,user_name,permission,pg_id,print_id, ';
$sql .= 'bg_color,font_color,font_size,';
$sql .= 'reverse_bg_color,reverse_font_color ';
$sql .= ',authority ';
$sql .= 'FROM M_USER ORDER BY user_id';
//クエリの準備
$sth = $dbh->prepare($sql);
//クエリの実行
$sth->execute();
// //DB切断
// $dbh = NULL;
//ログインユーザのユーザ設定格納
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	if($_SESSION['user_id'] === $row['user_id']){
		$loginUser = $row;
	}
}
//ログインユーザのユーザ設定取得失敗
if(empty($loginUser) ){
	$mess = 'DBからユーザ情報取得に失敗しました';
	errOutPut($mess);
}

//*********************************************ユーザ設定開始 add koyama  20160330
$clsLoginUser = New clsUserCondition($loginUser);

//*********************************************ユーザに対する権限取得 add koyama 20160330
//pg名の取得
$pgArray = array();
$sql = 'SELECT pg_id,pg_name,permission ';
$sql .= 'FROM M_PG ';
$sql .= ';';
//クエリの準備
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}

//クエリの実行
$sth->execute();

//各行を変数に格納
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	$pgArray[] =$row;
}

$clsLoginUser->setPgList($pgArray);


// $oLog->info(' ^ - ^ '. print_r($postArray,true).__FILE__.':'.__LINE__);
//*********************************************↓更新********** or Ajax **********
//コンボボクスから値が送られてくることを確認
if(isset($postArray) && count($postArray) > 0){
	//add koyama Ajax用の処理を
	$oLog->info('query error !!'. print_r($postArray,true).__FILE__.':'.__LINE__);
	if(array_key_exists('ajax_flg',$postArray) && $postArray['ajax_flg'] == 1){
		//取得対象ユーザのユーザ設定
		$getUser = array();
		// $oLog->info('query error !!'. print_r($postArray,true).__FILE__.':'.__LINE__);
		if (strcmp($loginUser['user_id'], $postArray['user_id']) !== 0) {
			//エラーになったら空で返す
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
		//クエリの準備
		$sth = $dbh->prepare($sql);
		$sth->bindParam(':user_id',$postArray['chuser_id']);
		if (!$sth) {
			$oLog->info('query error !!'.__FILE__.':'.__LINE__);
			//エラーになったら空で返す
			exit;
		}
		//クエリの実行
		$sth->execute();
		//取得対象ユーザ情報の格納
		while($row = $sth->fetch(PDO::FETCH_ASSOC)){
			//1行しか無いはず
			$getUser = $row;
		}
		//取得失敗
		if(empty($getUser) ){
			//エラーになったら空で返す
			$oLog->info('not found target user !!'.__FILE__.':'.__LINE__);
			exit;
		}
		$tempArray = $clsLoginUser->splitPermissionString($getUser['permission']);
		$oLog->info('query error !!'. print_r($tempArray,true).__FILE__.':'.__LINE__);
		//staticのように使用
		foreach($tempArray as $key => $part){
			$getUser[$key] = $part;
		}

		echo json_encode($getUser);
 		exit;
	}else{
		//add koyama ログインユーザ権限をここに
		//ログインユーザ権限確認
		if($clsLoginUser->authority == 0){
		//**********↓一般ユーザ確認**********
			//ユーザＩＤ確認
			if (strcmp($loginUser['user_id'], $postArray['user_id']) !== 0) {
				$mess = 'ユーザＩＤが不正なので更新できません';
				errOutPut($mess);
			}
			//ユーザ名確認
			if (strcmp($loginUser['user_name'], $postArray['user_name']) !== 0) {
				$mess = 'ユーザ名が不正なので更新できません';
				errOutPut($mess);
			}

		}
		//**********↑一般ユーザ確認終了**********
		//**********↓全ユーザ確認**********
		//背景色確認
		if(!array_key_exists($postArray['bg_color'],$colorArray)){
			$mess = '背景色が不正なので更新できません';
			errOutPut($mess);
		}
		//文字色確認
		if(!array_key_exists($postArray['font_color'],$colorArray)){
			$mess = '文字色が不正なので更新できません';
			errOutPut($mess);
		}
		//文字サイズ確認
		if($postArray['font_size'] < MIN_F_SIZE or $postArray['font_size'] > MAX_F_SIZE){
			$mess = '文字サイズが不正なので更新できません';
			errOutPut($mess);
		}
		//強調背景色確認
		if(!array_key_exists($postArray['reverse_bg_color'],$colorArray)){
			$mess = '強調背景色が不正なので更新できません';
			errOutPut($mess);
		}
		//強調文字色確認
		if(!array_key_exists($postArray['reverse_font_color'],$colorArray)){
			$mess = '強調文字色が不正なので更新できません';
			errOutPut($mess);
		}
		//**********↑全ユーザ確認終了**********
		//UPDATE用の値の作成
		$strPermission = $clsLoginUser->concatPermissionString($postArray);

		//SQL文(UPDATE)の準備
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
		//クエリの準備
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
		//クエリの実行
		$sth->execute();
		$oLog->info('query !!' . $sql.' : '. print_r($postArray,true).__FILE__.':'.__LINE__);
	}
}
//**********↑更新終了**********
$oLog->info(' ^ - < '.__FILE__.':'.__LINE__);
$sql = 'SELECT user_id,user_name,permission,pg_id,print_id, ';
$sql .= 'bg_color,font_color,font_size,';
$sql .= 'reverse_bg_color,reverse_font_color ';
$sql .= ',authority ';
$sql .= 'FROM M_USER ORDER BY user_id';
//クエリの準備
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}
//クエリの実行
$sth->execute();


//ログインユーザのユーザ設定格納
while($row = $sth->fetch(PDO::FETCH_ASSOC)){
	array_push($resultArray,$row);
	if($_SESSION['user_id'] === $row['user_id']){
		$loginUser = $row;
	}
}
//ログインユーザのユーザ設定取得失敗
if(empty($loginUser) ){
	$mess = 'DBからユーザ情報取得に失敗しました';
	errOutPut($mess);
}
//**********全ユーザ設定取得終了**********

//********************************************更新された値がある可能性があるため入れなおし
$clsLoginUser->resetProp($loginUser);

//*********************************************プリンタのリスト取得 add koyama 20160401
//printer名の取得
$printArray = array();
$sql = 'SELECT print_id,print_name ';
$sql .= 'FROM M_PRINTER ';
$sql .= ';';
//クエリの準備
$sth = $dbh->prepare($sql);
if (!$sth) {
	$oLog->info('query error !!'. print_r($dbh->errorInfo(),true).__FILE__.':'.__LINE__);
}
//クエリの実行
$sth->execute();

//各行を変数に格納
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
