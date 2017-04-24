	<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$commands = array();                          //取得した条件タグ一覧を格納
$Del_Flg       = '0';                         //削除フラグを格納
$today         = date("Y-m-d H:i:s");         //サーバの日付取得
$Mod_Date      = $today;                      //変更日を格納
$post          = $_POST;                      //post値を格納
$successCode   = '';                          //成功メッセージコードを格納
$message_codes = '';                          //エラーメッセージコードを格納

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
	//POSTが存在している時
	if(!empty($post['action']) && $post['action']=="del" ){

		//削除のとき
		$Command_Id = $post['Command_Id'];

		//SQL作成（アップデート）
		$sql = " UPDATE t_command SET Del_Flg='1',Mod_Date=:Mod_Date WHERE Command_Id=:Command_Id AND User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Command_Id',$Command_Id);
		$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
		$sth->bindParam('Mod_Date',$Mod_Date);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL実行
		$sth->execute();

		//SQL作成（アップデート確認）
		$sql = " SELECT Command_Id,User_Id,Del_Flg FROM t_command WHERE Command_Id=:Command_Id AND User_Id=:User_Id AND Del_Flg='1' ;";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('Command_Id',$Command_Id);
		$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
		//SQL実行
		$sth->execute();
		$loopCount = 0;
		while($command = $sth->fetch(PDO::FETCH_ASSOC)){
			$loopCount++;
		}
		if($loopCount!=0){
			$successCode   = 'INF0006';
		}else{
			//アップデート確認でレコードが取得できなかった場合
			addCode($message_codes,'ERR0015');
			put_error('ERR0015',$e);
		}
	}
}

//条件タグ一覧取得
get_commands($commands,$message_codes,$dbhDV);

//画面描画
require_once('./view/vw_DataView_command_search.php');

?>
