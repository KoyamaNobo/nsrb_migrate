<?php
if(!isset($_SESSION)){
	session_start();
}

//ログファイル読み込み
require_once('./lib/log.php');
require_once('./lib/config.php');

//changeHttps();

//変数定義
$get = $_GET;
$post = $_POST;
$file = "";
$messages = array();
$updateerr="0";

if(!isset($oConf)){
	$oConf	= New initConf();					//config.php内コンフィグ用クラス
}

//postが無いときは
if(!isset($post) || empty($post)){
	$file['name'] = $get['filename'];
	$file['name_befor'] = $get['filename'];
}else{
	$file['name'] = $post['filename'];
	$file['name_befor'] = $post['filename_befor'];
}

//セッション確認
if(!(isset($_SESSION['user_id']) && !empty($_SESSION['user_id']))){
	//エラー処理
	$mess = 'ユーザＩＤが不正の為、更新できません';
	errOutPut($mess);
}

if(isset($post) && !empty($post)){
	//postの値が存在している時
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'update'){
		//ファイル名更新時
		//ファイル名チェック
		if(preg_match('/[\\\\\/:*?"<>|\']/',$file['name'])){
			array_push($messages, 'ファイル名に次の文字は使用できません。\/:*?"<>|\'</br>');
			$updateerr="1";
		}
		
		//ファイル名に問題がない時
		if($updateerr != "1"){
			if(!rename($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8","cp932"),
					   $oConf->getstrExplorerPath() . mb_convert_encoding($file['name'],"utf8", "cp932" ))){
				//ファイルのリネームに失敗したとき
				array_push($messages, 'ファイル名の変更に失敗しました。</br>');
			}else{
				//ファイルのリネームに成功したとき
				array_push($messages, 'ファイル名を更新しました。</br>');
				$file['name_befor'] = $file['name'];
			}
		}
	}
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'download'){
		//ダウンロード時
		ini_set('memory_limit', '3000M');
		header('Content-Disposition: attachment; filename="' . $file['name_befor'] . '"');
		header('Content-Type: application/octet-stream');
		header('Content-Transfer-Encoding: binary');
		header('Content-Length: ' . filesize($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932")));
		
		if(!readfile($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"))){
			array_push($messages, 'ファイルのダウンロードに失敗しました。</br>');
		}
		exit;
	}
	if(isset($post['action']) && !empty($post['action']) && $post['action'] == 'remove'){
		//ファイル削除
		if(!unlink($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"))){
			array_push($messages, 'ファイルの削除に失敗しました。</br>');
		}else{
			//エラーメッセージ受け渡し
			$tmp = urlencode("ファイルを削除しました。</br>");
			if(isset($nextExeName) && !empty($nextExeName)){
				header('Location: ./'.$nextExeName.'?message=' .$tmp);
			}else{
				header('Location: ./'.'DataExchange'.'?message=' .$tmp);
			}
			exit;
		}
	}
}

//ファイルの更新時間取得
if(isset($file['name']) && !empty($file['name'])){
	$file['lastmod'] = date('Y/m/d H:i:s', filemtime($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932")));
	$file['filesize'] = filesize($oConf->getstrExplorerPath() . mb_convert_encoding($file['name_befor'],"utf8", "cp932"));
}

//view呼び出し
require_once('./view/vw_DataExchangeDetail.php');

?>