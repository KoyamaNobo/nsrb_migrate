<?php
if(!isset($_SESSION)){
	session_start();
}
$test = ini_set('max_file_uploads',50);
shell_exec('logger -i "DataExchange php max_file_uploads change'.print_r($test).'"');
//ログファイル読み込み
require_once('./lib/log.php');
require_once('./lib/config.php');

//changeHttps();

//変数定義
$get	  = $_GET;
$messages = array();
$files	= array();
$sort	 = "lastmodDESC";

if(!isset($oConf)){
	$oConf	= New initConf();					//config.php内コンフィグ用クラス
}

//セッション確認
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//エラー処理
	$mess = 'ユーザＩＤが不正の為、更新できません';
	errOutPut($mess);
}

if(isset($get) && !empty($get)){
	if(isset($get['message']) && !empty($get['message'])){
		array_push($messages, $get['message'] . "<br/>");
	}
	if(isset($get['sort']) && !empty($get['sort'])){
		$sort = $get['sort'];
	}
}

//ファイルが存在していればアップロード処理をする
if (isset($_FILES) && !empty($_FILES) && isset($_FILES['userfile']) && !empty($_FILES['userfile'])){
	$count = count($_FILES['userfile']['name']);
	for ($i=0; $i<$count; $i++) {
		if (is_uploaded_file($_FILES["userfile"]["tmp_name"][$i])) {
			if (move_uploaded_file($_FILES["userfile"]["tmp_name"][$i],
								   $oConf->getstrExplorerPath() . mb_convert_encoding($_FILES["userfile"]["name"][$i],"utf8", "cp932"))) {
				chmod($oConf->getstrExplorerPath() . mb_convert_encoding($_FILES["userfile"]["name"][$i],"utf8", "cp932"), 0644);
				array_push($messages, $i + 1 ."&nbsp;&nbsp;" . $_FILES["userfile"]["name"][$i] . "をアップロードしました。<br/>");
			} else {
				array_push($messages, $i + 1 ."&nbsp;&nbsp;" . $_FILES["userfile"]["name"][$i] . "をアップロードできません。<br/>");
			}
		} else {
			array_push($messages, "ファイルが選択されていません。<br/>");
		}
	}
}

//ファイル取得
$tmp = array();
if ($handle = opendir($oConf->getstrExplorerPath())) {
	while (false !== ($file = readdir($handle))) {
		if ($file !== "." && $file !== ".."){
			if($oConf->getintExtState() == 1 && (preg_match('/^'.TEMP_FILE_PREFIX.'/',$file) || preg_match('/^'.TEMP_PDF_PREFIX.'/',$file)) ){
				continue;
			}
			$tmp['name'] = $file;
			$tmp['lastmod'] = date('Y/m/d H:i:s', filemtime($oConf->getstrExplorerPath() . $file));
			$tmp['filesize'] = filesize($oConf->getstrExplorerPath() . $file);
			array_push($files, $tmp);
		}
	}
	closedir($handle);
	
	//並び替え追加
	if($sort=="nameDESC"){
		usort($files, function($a, $b) {
			return ($a['name']) < ($b['name']) ? 1:-1;
		});
	}
	if($sort=="nameASC"){
		usort($files, function($a, $b) {
			return ($a['name']) < ($b['name']) ? -1:1;
		});
	}
	if($sort=="lastmodDESC"){
		usort($files, function($a, $b) {
		   return strtotime($a['lastmod']) < strtotime($b['lastmod']) ? 1:-1;
		});
	}
	if($sort=="lastmodASC"){
		usort($files, function($a, $b) {
			return strtotime($a['lastmod']) < strtotime($b['lastmod']) ? -1:1;
		});
	}
	if($sort=="sizeDESC"){
		usort($files, function($a, $b) {
			return ($a['filesize']) < ($b['filesize']) ? 1:-1;
		});
	}
	if($sort=="sizeASC"){
		usort($files, function($a, $b) {
			return ($a['filesize']) < ($b['filesize']) ? -1:1;
		});
	}
}
//view呼び出し
require_once('./view/vw_DataExchange.php');

?>