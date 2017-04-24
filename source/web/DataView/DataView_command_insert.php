<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post           = $_POST;
$today          = date("Y-m-d H:i:s");
$Cre_Date       = $today;
$Mod_Date       = $today;
$Del_Flg        = '0';
$message_codes  = '';

//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	echo $message_codes."\r\n";
	return false;
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	echo $message_codes."\r\n";
	return false;
}

if(empty($post) || empty($post['Command_Name']) ){
	//postパラメータが存在しないとき
	echo "ERR0701\r\n";
	put_error('ERR0701',$e);
	return false;
}

//SQL文作成（インサート文）
$sql  = " INSERT INTO t_command (Command_Id,Command_Name,User_Id,Sql_str,Cre_Date,Mod_Date,Del_Flg ) ";
$sql .= " VALUES( DEFAULT,:Command_Name,:User_Id,:Sql_str,:Cre_Date,:Mod_Date,:Del_Flg) ;";

//SQL文実行（インサート文）
$sth = $dbhDV->prepare($sql);
$tmp = mb_convert_encoding($post['Command_Name'], "SJIS", "UTF-8");
$sth->bindParam('Command_Name',$tmp);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
$sth->bindParam('Sql_str',$post['Sql_str']);
$sth->bindParam('Cre_Date',$Cre_Date);
$sth->bindParam('Mod_Date',$Mod_Date);
$sth->bindParam('Del_Flg',$Del_Flg);
//SQL実行
$sth->execute();

//SQL作成（インサート確認）
$sql  = " SELECT  *  ";
$sql .= " FROM t_command  ";
$sql .= " WHERE Command_Name = :Command_Name ";
$sql .= " AND User_Id = :User_Id ";
$sql .= " AND Sql_str = :Sql_str ";
$sql .= " AND Del_Flg = :Del_Flg ";
//SQL文実行（インサート確認）
$sth = $dbhDV->prepare($sql);
$tmp = mb_convert_encoding($post['Command_Name'], "SJIS", "UTF-8");
$sth->bindParam('Command_Name',$tmp);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
$sth->bindParam('Sql_str',$post['Sql_str']);
$sth->bindParam('Del_Flg',$Del_Flg);
//SQL実行
$sth->execute();

//１件以上存在していることを確認
$count = 0;
while($command = $sth->fetch(PDO::FETCH_ASSOC)){
	$count++;
}
if($count > 0){
	//インサート成功時
	echo "INF0001\r\n";
}else{
	//インサート失敗時
	echo "ERR0016\r\n";
	put_error('ERR0016','');
}

return;
?>
