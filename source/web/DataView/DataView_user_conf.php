<?php
session_start();

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

//変数初期化
$post          = $_POST;
$today         = date("Y-m-d H:i:s");
$userAuthority = '';                            //ログインしているユーザの権限
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


//DB接続（DataView用コネクション:$dbhDVの作成）
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

//ログインチェック
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}


//SQL文作成（アップデート確認用）
$sql  = " SELECT Authority_Flg ";
$sql .= " FROM m_user ";
$sql .= " WHERE ";
$sql .= "     User_Id=:User_Id ; ";
$sth = $dbhDV->prepare($sql);
$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
//SQL実行
$sth->execute();

//１件しか無いはず
$count = 0;
while($user = $sth->fetch(PDO::FETCH_ASSOC)){
	$userAuthority = $user['Authority_Flg'];
}


//登録or削除or更新or更新画面呼び出し処理
if(!empty($post)){
	if(!empty($post['action']) && $post['action']=="add" ){

		//POSTデータ取得
		$User_Id       = $post['User_Id'];
		$User_Password = $post['User_Password'];
		$User_Conf     = $post['User_Conf'];
		shell_exec('logger -i "authority '.$post['Authority_Flg'].'"');
		$Authority_Flg = $post['userAuthority'];
		$Disp_Num      = $post['Disp_Num'];
		$Del_Flg       = '0';

		//SQL文作成（インサート文）
		$sql  = " INSERT INTO m_user (User_Id,User_Password,Authority_Flg,Disp_Num,Cre_Date,Mod_Date,Del_Flg ) ";
		$sql .= " VALUES( :User_Id,:User_Password,:Authority_Flg,:Disp_Num,now(),now(),:Del_Flg) ;";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('User_Password',$User_Password);
		$sth->bindParam('Authority_Flg',$Authority_Flg);
		$sth->bindParam('Disp_Num',$Disp_Num);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL実行
		$sth->execute();

		//SQL文作成（インサート確認用）
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
		//SQL実行
		$sth->execute();

		//１件以上存在していることを確認
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}

		if($count > 0){
			//インサート成功時
			//画面描画用変数初期化
			$User_Id       = '';
			$User_Password = '';
			$Authority_Flg = '';
			$Disp_Num      = '';
			$successCode   = 'INF0002';
		}else{
			//インサート失敗時
			addCode($message_codes,'ERR0010');
			put_error('ERR0010','');
		}

	}else if(!empty($post['action']) && $post['action']=="del" ){

		//POSTデータ取得
		$User_Id       = $post['User_Id'];
		$Del_Flg       = '0';

		//SQL文作成（削除前確認用）
		$sql = " SELECT User_Id,Del_Flg FROM m_user WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL実行
		$sth->execute();
		//１件以上存在していることを確認
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}
		if($count == 0){
			//事前確認失敗時
			addCode($message_codes,'ERR0011');
			put_error('ERR0011','');
		}

		if($message_codes == ''){
			//SQL文作成（削除用）
			$sql = " UPDATE m_user SET Del_Flg='1',Mod_Date=:Mod_Date WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
			$sth = $dbhDV->prepare($sql);
			$sth->bindParam('User_Id',$User_Id);
			$sth->bindParam('Mod_Date',$Mod_Date);
			$sth->bindParam('Del_Flg',$Del_Flg);
			//SQL実行
			$sth->execute();

			//SQL文作成（削除確認用）
			$sql  = " SELECT * ";
			$sql .= " FROM m_user ";
			$sql .= " WHERE ";
			$sql .= "     User_Id=:User_Id AND ";
			$sql .= "     Mod_Date=:Mod_Date AND ";
			$sql .= "     Del_Flg='1' ; ";
			$sth = $dbhDV->prepare($sql);
			$sth->bindParam('User_Id',$User_Id);
			$sth->bindParam('Mod_Date',$Mod_Date);
			//SQL実行
			$sth->execute();
			//１件以上存在していることを確認
			$count = 0;
			while($user = $sth->fetch(PDO::FETCH_ASSOC)){
				$count++;
			}
			if($count > 0){
				//正常終了したら左の値を削除する
				$User_Id       = '';
				$successCode   = 'INF0003';
			}else{
				//削除失敗時
				addCode($message_codes,'ERR0012');
				put_error('ERR0012','');
			}
		}
	}else if(!empty($post['action']) && $post['action']=="detail" ){

		//POSTデータ取得
		$User_Id       = $post['User_Id'];
		$Del_Flg       = '0';

		//SQL作成（ユーザ取得用）
		$sql = " SELECT User_Id,User_Password,Authority_Flg,Disp_Num,Del_Flg FROM m_user WHERE User_Id=:User_Id AND Del_Flg=:Del_Flg; ";
		$sth = $dbhDV->prepare($sql);
		$sth->bindParam('User_Id',$User_Id);
		$sth->bindParam('Del_Flg',$Del_Flg);
		//SQL実行
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
			//ユーザ更新画面を表示して終了
			require_once('./view/vw_DataView_User_Conf_Mod.php');
			exit;
		}else{
			//削除失敗時
			addCode($message_codes,'ERR0014');
			put_error('ERR0014','');
		}
	}else if(!empty($post['action']) && $post['action']=="mod" ){
		//更新時
		//POSTデータ取得
		$User_Id       = $post['User_Id'];
		$User_Password = $post['User_Password'];
		$User_Conf     = $post['User_Conf'];
		$Authority_Flg = $post['Authority_Flg'];
		$Disp_Num      = $post['Disp_Num'];
		$Del_Flg       = '0';

		shell_exec('logger -i "^^^Authority_Flg'.$Authority_Flg.'"');

		//SQL文作成（アップデート文）
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
		//SQL実行
		shell_exec('logger -i "^^^$sql'.$sql.'"');
		shell_exec('logger -i "^^^Del_Flg'.$Del_Flg.'"');
		$sth->execute();

		//SQL文作成（アップデート確認用）
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
		//SQL実行
		$sth->execute();


		//１件以上存在していることを確認
		$count = 0;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$count++;
		}
		if($count > 0){
			if($_SESSION['dv_user_id'] == $User_Id){
				//ログインIDと同一のユーザIDを更新した場合セッション情報をすべて書き換える
				$_SESSION['dv_user_id']            = $User_Id;
				if($User_Password != ''){
					$_SESSION['dv_user_password']      = $User_Password;
				}
				$_SESSION['dv_user_authority_flg'] = $Authority_Flg;
				$_SESSION['dv_user_disp_num']      = $Disp_Num;
			}
			//正常終了したら左の値を削除する
			$User_Id       = '';
			$User_Password = '';
			$Authority_Flg = '';
			$Disp_Num      = '';
			//出力用メッセージコード
			$successCode   = 'INF0004';
		}else{
			//削除失敗時
			addCode($message_codes,'ERR0013');
			put_error('ERR0013',$e);
		}
	}
}

//ユーザー一覧取得
get_users($users,$message_codes,'0',$dbhDV);

$i=0;
//画面に表示するため権限変換
foreach($users as $user){
	if($user['Authority_Flg'] == '1'){
		$users[$i]["A_Flg_Disp"] = '有り';
	}else{
		$users[$i]["A_Flg_Disp"] = '無し';
	}
	$i++;
}

//画面描画
require_once('./view/vw_DataView_User_Conf.php');

?>
