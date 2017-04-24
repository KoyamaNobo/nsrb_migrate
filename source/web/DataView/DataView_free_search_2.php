<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post           = $_POST;
$tables         = array(); //ポストデータからテーブル名
$selectedBonds  = array(); //戻ってきたときの結合条件
$selectedTables = array();
$message_codes  = '';

//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}


if(!empty($post)){
	//選択テーブル取得
	foreach($post['selectedTables'] as $tmp){
		array_push($selectedTables,  trim($tmp));
	}

	//結合条件取得
	if(isset($post['selectedBonds'])){
		//データ初期変換
		foreach($post['selectedBonds'] as $tmp){
			//AHNHF.AHNH-R~~1~64~~LEFTJOIN~B-TCM.BM-KEY~~1~4~
			$tmps  = explode("~", $tmp);//「~」で分割
			$ltmps = explode(".", $tmps[0]);
			$rtmps = explode(".", $tmps[6]);
			$add   = array();
			$add['lListTableName']     = trim($ltmps[0]);
			$add['lListItemName']      = trim($ltmps[1]);
			if(trim($tmps[1]) == ""){
				$add['lListJapaneseName']  = " "; //スペースを設定することで表示時には「&nbsp;」と表示される
			}else{
				$add['lListJapaneseName']  = trim($tmps[1]);
			}
			$add['lListS_point']       = trim($tmps[2]);
			$add['lListSize']          = trim($tmps[3]);
			$add['lListType']          = trim($tmps[4]);
			if($tmps[5]=="LEFTJOIN"){
				$add['bondConditions']     = "左を元に結合";
			}else{
				$add['bondConditions']     = "互いに一致のみ";
			}
			$add['rListTableName']     = trim($rtmps[0]);
			$add['rListItemName']      = trim($rtmps[1]);
			if(trim($tmps[7]) == ""){
				$add['rListJapaneseName']  = " "; //スペースを設定することで表示時には「&nbsp;」と表示される
			}else{
				$add['rListJapaneseName']  = trim($tmps[7]);
			}
			$add['rListS_point']       = trim($tmps[8]);
			$add['rListSize']          = trim($tmps[9]);
			$add['rListType']          = trim($tmps[10]);
			$add['key']                = trim($tmps[0]) ."~".trim($tmps[6]);
			$add['id']                 = trim($tmp);
			array_push($selectedBonds, $add);
		}
	}
}else{
	//URLのみで呼び出された時
	addCode($message_codes,'ERR0501');
	put_error('ERR0501',$e);
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//画面描画
require_once('./view/vw_DataView_Free_Search_2.php');

?>
