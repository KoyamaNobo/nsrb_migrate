<?php

//メッセージ
$MESSAGE = array(
    "ERR0001"    => "DB接続に失敗しました。",
    "ERR0002"    => "ログインに失敗しました。",
    "ERR0003"    => "テーブル一覧の取得に失敗しました。",
    "ERR0004"    => "項目情報が存在していません。",
    "ERR0005"    => "現在存在していないテーブルが選択されていたため選択テーブルは初期化しました。",
    "ERR0006"    => "ユーザが存在していません。",
    "ERR0007"    => "条件タグの登録に失敗しました。",
    "ERR0008"    => "条件タグが登録されていません。",
    "ERR0009"    => "条件タグ取得に失敗しました。",
    "ERR0010"    => "ユーザ登録に失敗しました。",
    "ERR0011"    => "削除対象のユーザがDBに存在していません。",
    "ERR0012"    => "ユーザ削除に失敗しました。",
    "ERR0013"    => "ユーザ更新に失敗しました。",
    "ERR0014"    => "ユーザ取得に失敗しました。",
    "ERR0015"    => "条件タグ削除に失敗しました。",
    "ERR0016"    => "条件タグ登録に失敗しました。",
    "ERR0017"    => "項目の更新に失敗しました。",
    "ERR0018"    => "項目の作成に失敗しました。",
    "ERR0019"    => "ファイルのダウンロードに失敗しました。",
    "ERR0020"    => "項目の削除に失敗しました。",
    "ERR0501"    => "不正なアクセスです。",
    "ERR0601"    => "パラメータエラーです。",
    "ERR0701"    => "条件タグ登録のパラメータエラーです。",

    "INF0001"    => "条件タグを登録しました。",
    "INF0002"    => "ユーザを登録しました。",
    "INF0003"    => "ユーザを削除しました。",
    "INF0004"    => "ユーザを更新しました。",
    "INF0005"    => "和名を更新しました。",
    "INF0006"    => "条件タグを削除しました。",
    "INF0007"    => "項目を削除しました。",
    "INF0008"    => "項目を更新しました。",


    "ERRJ0001"    => "ユーザIDは必須入力です。",
    "ERRJ0002"    => "ユーザIDは英数のみ（記号不可）で入力してください。",
    "ERRJ0003"    => "ユーザIDは４文字以上１６文字以内としてください。",
    "ERRJ0004"    => "パスワードは必須入力です。",
    "ERRJ0005"    => "パスワードは英数のみ（記号不可）で入力してください。",
    "ERRJ0006"    => "パスワードは４文字以上１６文字以内としてください。",
    "ERRJ0007"    => "表示件数は必須入力です。",
    "ERRJ0008"    => "表示件数は数値で入力してください。",
    "ERRJ0009"    => "表示件数は１桁～２桁で入力してください。",
    "ERRJ0010"    => "表示件数は１５～９９で入力してください。",
    "ERRJ0011"    => "IDが重複しているため登録できません。",
    "ERRJ0012"    => "確認用パスワードが一致していません。",
    "ERRJ0013"    => "編集するユーザが選択されていません。",
    "ERRJ0014"    => "削除するユーザが選択されていません。",
    "ERRJ0015"    => "権限がないため削除できません。",
    "ERRJ0016"    => "権限がないためログインユーザ以外のIDは編集できません。",
    "ERRJ0017"    => "権限がないため追加できません。",
    "ERRJ0018"    => "削除されているため編集できません。",
    "ERRJ0019"    => "ログインユーザ以外が作成した条件タグは削除できません。",
    "ERRJ0020"    => "和名は１文字～２０文字で入力してください。",
    "ERRJ0021"    => "和名に特殊記号は使用しないでください。",
    "ERRJ0022"    => "項目が選択されていません。",
    "ERRJ0023"    => "権限がないため設定を編集できません。",
    "ERRJ0024"    => "条件タグが選択されていません。",
    "ERRJ0025"    => "テーブルが選択されていません。",
    "ERRJ0026"    => "すでに選択テーブルに登録されています。",
    "ERRJ0027"    => "削除するテーブルが選択されていません。",
    "ERRJ0028"    => "選択テーブルは２つ以上設定してください。",
    "ERRJ0029"    => "結合条件が選択されていません。",
    "ERRJ0030"    => "テーブル１とテーブル２の候補を選択してください。",
    "ERRJ0031"    => "テーブル１とテーブル２は別のテーブルを選択してください。",
    "ERRJ0032"    => "結合条件の中に同じテーブルの組み合わせが既に存在しています。",
    "ERRJ0033"    => "結合条件を複数設定する場合は既に結合しているテーブルと結合させてください。",
    "ERRJ0034"    => "テーブル選択で選択したテーブルが結合条件として使用されていないものがあります。",
    "ERRJ0035"    => "表示項目が選択されていません。",
    "ERRJ0036"    => "候補が選択されていません。",
    "ERRJ0037"    => "表示項目が設定されていません。",
    "ERRJ0038"    => "項目名が選択されていません。",
    "ERRJ0039"    => "比較演算子が選択されていません。",
    "ERRJ0040"    => "値が入力されていません。",
    "ERRJ0041"    => "値の入力は半角数字のみ可能です（マイナス記号不可）。",
    "ERRJ0042"    => "値の桁数がサイズを超えています。",
    "ERRJ0043"    => "入力は半角数字のみ可能です（マイナス記号可）。",
    "ERRJ0044"    => "日本語に対して抽出条件は設定できません。",
    "ERRJ0045"    => "半角のみ入力可能です。",
    "ERRJ0046"    => "特殊記号「&lt;&gt;&amp;&quot;'`~」は入力不可です。",
    "ERRJ0047"    => "抽出条件が選択されていません。",
    "ERRJ0048"    => "並べ替えが選択されていません。",
    "ERRJ0049"    => "候補が選択されていません。",
    "ERRJ0050"    => "すでに同じ項目が設定されています。",
    "ERRJ0051"    => "すでに同じテーブル名・項目名が表示項目に設定されています。",
    "ERRJ0052"    => "保存する名前を入力してください。",
    "ERRJ0053"    => "特殊記号「&lt;&gt;&amp;&quot;'`~」は入力不可です。",
    "ERRJ0054"    => "現在CSV作成中ですしばらくお待ちください。",
    "ERRJ0055"    => "現在ページ遷移中ですしばらくお待ちください。",
    "ERRJ0056"    => "通信に失敗しました。",
    "ERRJ0057"    => "CSV作成ジョブの状態取得が失敗しました。",
    "ERRJ0058"    => "テーブル名に使用できない文字が含まれています。",
    "ERRJ0059"    => "編集する行が選択されていません。",
    "ERRJ0060"    => "削除する行が選択されていません。",
    "ERRJ0061"    => "型が正しくありません。",
    "ERRJ0062"    => "開始位置は数字で入力してください。",
    "ERRJ0063"    => "開始位置は1～4096で入力してください。",
    "ERRJ0064"    => "長さは数字で入力してください。",
    "ERRJ0065"    => "長さは1～4096で入力してください。",
    "ERRJ0066"    => "テーブル名は１文字～２０文字で入力してください。",
    "ERRJ0067"    => "項目名は１文字～２０文字で入力してください。",
    "ERRJ0068"    => "項目名に特殊記号は使用しないでください。",
    "ERRJ0069"    => "サイズは数字で入力してください。",
    "ERRJ0070"    => "サイズは1～4096で入力してください。",
    "Sentinel"    => "Sentinel"
);


////////////////////////////////////////////////////////////////////////////////
//DB接続
//  引数
//      IPアドレス
//      ポート番号
//      ユーザ名（MariaDB）
//      パスワード（MariaDB）
//      ※コネクション
//      ※メッセージコード
//      （※は参照渡し）
//  戻値
//      DB接続成功時：true
//      DB接続失敗時：false
////////////////////////////////////////////////////////////////////////////////
function db_connect($db_name,$db_host,$db_port,$db_user,$db_pass,&$dbh,&$message_codes){
	//変数初期化
	$result     = false;
	try{
		//コネクション作成
		$options = array(PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES SJIS',);
		$dbh = new PDO("mysql:dbname=". $db_name .";host=" . $db_host . ";port=".$db_port, $db_user, $db_pass, $options);
		$result     = true;
	}catch (Exception $e) {
		//DB接続に失敗したとき
		addCode($message_codes,'ERR0001');
		put_error('ERR0001',$e);
	}
	return $result;
}


////////////////////////////////////////////////////////////////////////////////
//ログインチェック
//  引数
//      IPアドレス
//      ポート番号
//      ユーザ名（MariaDB）
//      パスワード（MariaDB）
//      ※コネクション
//      ※メッセージコード
//      （※は参照渡し）
//  戻値
//      DB接続成功時：true
//      DB接続失敗時：false
////////////////////////////////////////////////////////////////////////////////
function loginCheck($id, $pass ,&$message_codes,&$dbhDV){
	//変数初期化
	$result             = false;
	$user_id            = '';
	$user_password      = '';
	$user_authority_flg = '';
	$user_disp_num      = '';
	$Del_Flg            = '0';

	//ユーザの存在チェック
	$sql  = ' SELECT User_Id,User_Password,Authority_Flg,Disp_Num ';
	$sql .= ' FROM m_user ';
	$sql .= ' WHERE User_Id=:User_Id AND User_Password=:User_Password AND Del_Flg=:Del_Flg; ';
	$sth = $dbhDV->prepare($sql);
	$sth->bindParam('User_Id',$id);
	$sth->bindParam('User_Password',$pass);
	$sth->bindParam('Del_Flg',$Del_Flg);
	//SQL実行
	$sth->execute();
	while($user = $sth->fetch(PDO::FETCH_ASSOC)){
		$user_id            = $user['User_Id'];
		$user_password      = $user['User_Password'];
		$user_authority_flg = $user['Authority_Flg'];
		$user_disp_num      = $user['Disp_Num'];
	}
	if(!empty($user_id)){
		//ユーザが存在しているとき
		$_SESSION['dv_user_id']            = $user_id;
		$_SESSION['dv_user_password']      = $user_password;
		$_SESSION['dv_user_authority_flg'] = $user_authority_flg;
		$_SESSION['dv_user_disp_num']      = $user_disp_num;
		$result = true;
	}else{
		//ユーザが存在していないとき
		addCode($message_codes,'ERR0002');
		put_error('ERR0002',"");
	}
	return $result;
}

////////////////////////////////////////////////////////////////////////////////
//テーブル名の一覧取得
////////////////////////////////////////////////////////////////////////////////
function get_all_tables(&$tables,&$message_codes,&$dbhNIS){
	global $NOTDISPTABLE;
	if(!isset($NOTDISPTABLE)){
		$NOTDISPTABLE = array();
	}

	$result = false;
	//テーブル名の一覧取得
	$roopCount = 0;
	$tmpstr = '';
	$tmpstr = '"'.implode('","', $NOTDISPTABLE).'"';
	$sql  = 'SHOW TABLE STATUS FROM '.NIS_DB_NAME.' WHERE NOT comment LIKE "%VIEW%" AND NOT Name LIKE "M_%" AND Name NOT IN ('.$tmpstr.');';

	$sth = $dbhNIS->prepare($sql);
	//SQL実行
	$sth->execute();
	$roopCount = 0;
	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){
		//ワーク以外のテーブル取得
		$tb = array();
		$tb["id"]         = $roopCount;
		$tb["TableName"]  = $tmp['Name'];
		array_push($tables, $tb);
		$roopCount++;
		$result = true;
	}
	if($roopCount == 0){
		//テーブルが１件も存在していないとき
		addCode($message_codes,'ERR0003');
		put_error('ERR0003',"");
	}
	return $result;
}

////////////////////////////////////////////////////////////////////////////////
//M_ITEMELEMENTの一覧取得
//ref:conf_list_add_ajax.php,item_conf.php
////////////////////////////////////////////////////////////////////////////////
function get_items(&$items,&$message_codes,&$dbhNIS,$num){
	$result    = false;
	$roopCount = 0;

	//M_ITEMELEMENTより類似検索
	$sql  = ' SELECT DISTINCT ';
	$sql .= '     SUBSTRING_INDEX(x.TableName, "_", 1) AS TN , ';
	$sql .= '     x.Label, ';
	$sql .= '     x.S_Point, ';
	$sql .= '     x.Size, ';
	$sql .= '     x.DataType, ';
	$sql .= '     y.Japanese_Name, ';
	$sql .= '     y.NonDisp_Flg ';
	$sql .= ' FROM `'.NIS_DB_NAME.'`.M_ITEMELEMENT  AS x ';
	$sql .= '     LEFT JOIN `'.DB_NAME.'`.m_japanese_name  AS y ';
	$sql .= '         ON SUBSTRING_INDEX(x.TableName, "_", 1) = y.TableName AND ';
	$sql .= '            x.Label     = y.Label     AND ';
	$sql .= '            x.S_Point   = y.S_Point   AND ';
	$sql .= '            x.Size      = y.Size      AND ';
	$sql .= '            x.DataType  = y.DataType ';
	$sql .= ' WHERE y.Del_Flg = "0" or y.Del_Flg is NULL ';
	$sql .= ' ORDER BY TN ,S_Point,Size,Japanese_Name ';
	$sql .= ' LIMIT '.$_SESSION['dv_user_disp_num'] * $num.','.$_SESSION['dv_user_disp_num'].' ';
	$sql .= ' ; ';
	$sth = $dbhNIS->prepare($sql);
	//SQL実行
	$sth->execute();
	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){

		$item = array();
		$item["Id"]            = $tmp['TN'].'~'.$tmp['Label'];
		$item["TableName"]     = $tmp['TN'];
		$item["Label"]         = $tmp['Label'];
		$item["S_Point"]       = $tmp['S_Point'];
		$item["Size"]          = $tmp['Size'];
		if(empty($tmp['DataType']) || $tmp['DataType']==""){
			$item["DataType"]      = ' ';//スペースを入れることで表示時は「&nbsp;」に変換される
		}else{
			$item["DataType"]      = $tmp['DataType'];
		}
		if(empty($tmp['Japanese_Name']) ||  $tmp['Japanese_Name']==""){
			$item["Japanese_Name"]      = ' ';//スペースを入れることで表示時は「&nbsp;」に変換される
		}else{
			$item["Japanese_Name"]      = $tmp["Japanese_Name"];
		}
		if(empty($tmp['NonDisp_Flg']) || $tmp['NonDisp_Flg']==""){
			$item["NonDisp_Flg"]      = ' ';//スペースを入れることで表示時は「&nbsp;」に変換される
		}else{
			$item["NonDisp_Flg"]      = '非表示';
		}

		array_push($items, $item);
		$roopCount++;
	}
	if($roopCount == 0){
		//項目が１件も存在していないとき
		//（M_ITEMELEMENTには必ずレコードが存在しているためエラーとする）
		addCode($message_codes,'ERR0004');
		put_error('ERR0004',"");
	}else{
		//項目が１件以上存在しているとき
		$result = true;
	}
	return $result;
}

////////////////////////////////////////////////////////////////////////////////
//m_userの一覧取得
////////////////////////////////////////////////////////////////////////////////
function get_users(&$users,&$message_codes,$Del_Flg,&$dbhDV){
	$result = true;
	//m_userの一覧取得
	$sql  = ' SELECT User_Id,User_Password,Authority_Flg,Disp_Num,Del_Flg ';
	$sql .= ' FROM m_user ';
	$sql .= ' WHERE User_Id <> "admin" ';  //adminユーザはログインできなくなった場合に使用する最後の手段
	$sql .= ' ORDER BY Del_Flg,User_Id; ';
	$sth = $dbhDV->prepare($sql);
	//SQL実行
	$sth->execute();
	$roopCount = 0;
	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){
		$user = array();
		$user["User_Id"]       = $tmp['User_Id'];
		$user["User_Password"] = $tmp['User_Password'];
		$user["Authority_Flg"] = $tmp['Authority_Flg'];
		$user["Disp_Num"]      = $tmp['Disp_Num'];
		$user["Del_Flg"]       = $tmp['Del_Flg'];
		if($tmp['Del_Flg']=='1'){
			$user["Del_Flg_Jpn"]   = '削除';
		}else{
			$user["Del_Flg_Jpn"]   = ' ';//スペースを設定することで表示には「&nbsp;」と表示される
		}

		array_push($users, $user);
		$roopCount++;
	}
	if($roopCount == 0){
		//項目が１件も存在していないとき
		addCode($message_codes,'ERR0006');
		put_error('ERR0006',"");
		$result = false;
	}
	return $result;
}

////////////////////////////////////////////////////////////////////////////////
//エラーログ出力（出力先：ERR_LOG_PATH）
//  引数
//      メッセージコード
//      エクセプション
////////////////////////////////////////////////////////////////////////////////
function put_error($message_code,$ex) {
	//変数読み込み
	global $MESSAGE;
	//出力メッセージ整形
	$message =  "\n";
	$message =  $message . "    Message_code => " . $message_code." \n";
	$message =  $message . "    Message      =>  " . $MESSAGE[$message_code]." \n";
	$message =  $message . "    Exception    =>  " . $ex." \n";
	if(!empty($_SESSION['dv_user_id'])){
		$message =  $message . "    user_id  =>  " . $_SESSION['dv_user_id']." \n";
	}
	$date = date('Y:m:d H:i:s');
	$log = "Date:".$date . " |ERROR   |" .$message;
	//ログ出力用のファイル存在チェック
	if(!file_exists(ERR_LOG_PATH)) {
		if(!touch(ERR_LOG_PATH)){
			//エラー用のファイル作成に失敗した場合はシスログに出力する
			syslog(LOG_ERR,$log);
			return;
		}
	}
	//エラーログ出力
	if(!error_log($log, 3, ERR_LOG_PATH)){
		//ログ出力に失敗した場合はシスログに出力
		syslog(LOG_ERR,$log);
	}
	return;
}

////////////////////////////////////////////////////////////////////////////////
//t_commandの一覧取得
////////////////////////////////////////////////////////////////////////////////
function get_commands(&$commands,&$message_codes,&$dbhDV){
	$result  = true;
	$Del_Flg = '0';

	//SQL文作成
	$sql  = ' SELECT Command_Id,Command_Name,User_Id,Sql_str ';
	$sql .= ' FROM t_command ';
	$sql .= ' WHERE Del_Flg=:Del_Flg  ';
	$sql .= ' ORDER BY Del_Flg,User_Id; ';
	if($sth = $dbhDV->prepare($sql)){}else{ addCode($message_codes,'ERR0009');put_error('ERR0009',$e); return false;}
	if($sth->bindParam('Del_Flg',$Del_Flg)){}else{ addCode($message_codes,'ERR0009');put_error('ERR0009',$e); return false;}
	//SQL文実行
	if($sth->execute()){}else{ addCode($message_codes,'ERR0009');put_error('ERR0009',$e); return false;}
	$roopCount = 0;
	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){
		$command = array();
		$command["Command_Id"]   = $tmp['Command_Id'];
		$command["Command_Name"] = $tmp['Command_Name'];
		$command["User_Id"]      = $tmp['User_Id'];
		$command["Sql_str"]      = $tmp['Sql_str'];

		array_push($commands, $command);
		$roopCount++;
	}
	if($roopCount == 0){
		//項目が１件も存在していないとき
		addCode($message_codes,'ERR0008');
		put_error('ERR0008',"");
		$result = true;
	}
	return $result;
}

////////////////////////////////////////////////////////////////////////////////
//結合条件の画面表示
////////////////////////////////////////////////////////////////////////////////
function outputSelectedBond($selectedBond){

		//AHNHF.AHNH-NHSN~~8~32~N~LEFTJOIN~B-TCM.BM-R~~1~512~
		$tmps  = explode("~", $selectedBond);
		$ltmps = explode(".", $tmps[0]);
		$rtmps = explode(".", $tmps[6]);
		$add   = array();
		$add['lListTableName']     = trim($ltmps[0]);
		$add['lListItemName']      = trim($ltmps[1]);

		if($tmps[1]==""){
			$add['lListJapaneseName']  = " "; //スペースを設定することで表示時には「&nbsp;」と表示される
		}else{
			$add['lListJapaneseName']  = trim($tmps[1]);
		}

		$add['lListS_point']       = trim($tmps[2]);
		$add['lListSize']          = trim($tmps[3]);
		$add['lListType']          = $tmps[4];

		if($tmps[5]=="LEFTJOIN"){
			$add['bondConditions']     = "左を元に結合";
		}else{
			$add['bondConditions']     = "互いに一致のみ";
		}
		$add['rListTableName']     = trim($rtmps[0]);
		$add['rListItemName']      = trim($rtmps[1]);
		$add['rListJapaneseName']  = trim($tmps[7]);
		$add['rListS_point']       = trim($tmps[8]);
		$add['rListSize']          = trim($tmps[9]);
		$add['rListType']          = $tmps[10];
		$add['key']                = trim($tmps[0]) ."~".trim($tmps[6]);
		$add['id']                 = trim($selectedBond);

		echo '<td class="sListTableName">'     .htmlEscape($add['lListTableName'])    .'</td>';
		echo '<td class="sListItemName">'      .htmlEscape($add['lListItemName'])     .'</td>';
		echo '<td class="sListJapaneseName">'  .htmlEscape($add['lListJapaneseName']) .'</td>';
		echo '<td class="sListS_point">'       .htmlEscape($add['lListS_point'])      .'</td>';
		echo '<td class="sListSize">'          .htmlEscape($add['lListSize'])         .'</td>';
		echo '<td class="sListBond">'          .htmlEscape($add['bondConditions'])    .'</td>';
		echo '<td class="sListTableName">'     .htmlEscape($add['rListTableName'])    .'</td>';
		echo '<td class="sListItemName">'      .htmlEscape($add['rListItemName'])     .'</td>';
		echo '<td class="sListJapaneseName">'  .htmlEscape($add['rListJapaneseName']) .'</td>';
		echo '<td class="sListS_point">'       .htmlEscape($add['rListS_point'])      .'</td>';
		echo '<td class="sListSize">'          .htmlEscape($add['rListSize'])         .'</td>';

}

////////////////////////////////////////////////////////////////////////////////
//表示項目の画面表示
////////////////////////////////////////////////////////////////////////////////
function outputSelectedItemName($selectedItemName){

		$tmps = explode("~", $selectedItemName);
		$tmps2 = explode(".", $tmps[1]);

		$add = array();
		$add["listTableName"]      = $tmps[0];
		$add["listItemName"]       = $tmps[1];
		$add["listJapaneseName"]   = $tmps[2];
		$add["listId"]             = $tmps[3];
		$add["listS_point"]        = $tmps[4];
		$add["listSize"]           = $tmps[5];
		$add["listType"]           = $tmps[6];
		$add["id"]                 = $tmps[3];

		echo '<td class="sListTableName">'.htmlEscape($add['listTableName']).'</td>';
		echo '<td class="sListItemName">' .htmlEscape($add['listItemName']) .'</td>';
		echo '<td class="sListJapaneseName">' .htmlEscape($add['listJapaneseName']) .'</td>';
		echo '<td class="sListS_point">'  .htmlEscape($add['listS_point'])  .'</td>';
		echo '<td class="sListSize">'     .htmlEscape($add['listSize'])     .'</td>';

}
////////////////////////////////////////////////////////////////////////////////
//表示項目の画面表示
////////////////////////////////////////////////////////////////////////////////
function outputLeftList($selectedItemName,$listTypeFlg){

	$tmps = explode("~", $selectedItemName);
	$tmps2 = explode(".", $tmps[1]);
	////AHNHF~HSH-NSU(2,8)~ ~HSHF.HSH-NSU(2,8)~255~4~S9

	$add = array();
	$add["listTableName"]    = $tmps[0];
	$add["listItemName"]     = $tmps[1];
	if($tmps[2] == ""){
		$add["listJapaneseName"] = " ";//スペースを設定することで表示時には「&nbsp;」と表示される
	}else{
		$add["listJapaneseName"] = $tmps[2];
	}
	$add["listId"]           = $tmps[3];
	$add["listS_point"]      = $tmps[4];
	$add["listSize"]         = $tmps[5];
	if($tmps[6]==""){
		$add["listType"]       = " ";//スペースを設定することで表示時には「&nbsp;」と表示される
	}else{
		$add["listType"]       = $tmps[6];
	}
	$add["id"]             = $tmps[3];

	//昇順設定の画面から呼び出された時は型を付ける
	$title = '';
	if(!$listTypeFlg){
		$title = ' title="型 : '.$add["listType"].'"';
	}

 	echo '<span class="hidden bgChItems"'.$title.'></span>';


 	echo '<span class="listTableName bgChItems"'.$title.'>'          .htmlEscape($add["listTableName"])    .'</span>';
 	echo '<span class="listItemName bgChItems"'.$title.'>'           .htmlEscape($add["listItemName"])     .'</span>';
 	echo '<span class="listJapaneseName bgChItems"'.$title.'>'       .htmlEscape($add["listJapaneseName"]) .'</span>';
 	echo '<span class="hidden listId bgChItems"'.$title.'>'          .htmlEscape($add["listId"])           .'</span>';
 	echo '<span class="listS_point right bgChItems"'.$title.'>'      .htmlEscape($add["listS_point"])      .'</span>';
 	echo '<span class="listSize right bgChItems"'.$title.'>'         .htmlEscape($add["listSize"])         .'</span>';
 	if($listTypeFlg){
 		echo '<span class="listType bgChItems"'.$title.'>'           .htmlEscape($add["listType"])         .'</span>';
 	}else{
 		echo '<span class="hidden listType bgChItems"'.$title.'>'    .htmlEscape($add["listType"])         .'</span>';
 	}
 	echo '<span class="hidden bgChItems id"'.$title.'>'              .htmlEscape($add["id"])               .'</span>';
}


////////////////////////////////////////////////////////////////////////////////
//抽出条件の画面表示
////////////////////////////////////////////////////////////////////////////////
function outputSelectedFilter($selectedFilter,$filterCount){

		//and~AHNHF~AHNH-KEY~~1~7~~<=~456~AHNHF.AHNH-KEY
		$tmps = explode("~", $selectedFilter);
		$add = array();
		$add["andOr"]     = $tmps[0];
		$add["tableName"] = $tmps[1];
		$add["itemName"]  = $tmps[2];
		if($tmps[6] == ""){
			$add["japaneseName"]  = " ";
		}else{
			$add["japaneseName"]  = $tmps[3]; //スペースを埋め込むことで表示では「&nbsp;」で表示される
		}
		$add["s_point"] = $tmps[4];
		$add["size"] = $tmps[5];

		if($tmps[6] == ""){
			$add["type"] = " "; //スペースを埋め込むことで表示では「&nbsp;」で表示される
		}else{
			$add["type"] = $tmps[6];
		}

		$add["operator"]  = $tmps[7];
		$add["value"]     = $tmps[8];
		$add["id"]        = $tmps[9];
		if($filterCount != 0){
			echo '<td class="sListAndOr">'       .htmlEscape($add['andOr'])        .'</td>';
		}else{
			echo '<td class="sListAndOr">&nbsp;</td>';
		}
		echo '<td class="sListTableName">'   .htmlEscape($add['tableName'])    .'</td>';
		echo '<td class="sListItemName">'    .htmlEscape($add['itemName'])     .'</td>';
		echo '<td class="sListJapaneseName">' .htmlEscape($add['japaneseName']) .'</td>';
		echo '<td class="sListS_point">'     .htmlEscape($add['s_point'])      .'</td>';
		echo '<td class="sListSize">'        .htmlEscape($add['size'])         .'</td>';
		echo '<td class="sListType">'        .htmlEscape($add['type'])         .'</td>';
		echo '<td class="sListOperator">'    .($add['operator'])               .'</td>';
		echo '<td class="sListValue">'       .($add['value'])        .'</td>';

}
////////////////////////////////////////////////////////////////////////////////
//抽出条件の画面表示
////////////////////////////////////////////////////////////////////////////////
function outputSelectedSort($selectedSort){

		//AHNHF~AHNH-KEY~~asc~1~7~~AHNHF
		$tmps = explode("~", $selectedSort);
		$add = array();
		$add["listTableName"]    = $tmps[0];
		$add["listItemName"]     = $tmps[1];
		$add["listJapaneseName"] = $tmps[2];
		$add["listId"]           = $tmps[7] . "." . $tmps[1];
		if($tmps[3] == "asc"){
			$add["listSort"]     = "昇順";
		}else{
			$add["listSort"]     = "降順";
		}
		$add["listS_point"]      = $tmps[4];
		$add["listSize"]         = $tmps[5];
		$add["listType"]         = $tmps[6];
		$add["id"]               = $tmps[7].".".$tmps[1];
		$add["listNext"]         = $selectedSort;

		echo '<td class="sListTableName">'    .htmlEscape($add['listTableName'])    .'</td>';
		echo '<td class="sListItemName">'     .htmlEscape($add['listItemName'])     .'</td>';
		echo '<td class="sListJapaneseName">' .htmlEscape($add['listJapaneseName']) .'</td>';
		echo '<td class="sListSort">'         .htmlEscape($add['listSort'])         .'</td>';
		echo '<td class="sListS_point">'      .htmlEscape($add['listS_point'])      .'</td>';
		echo '<td class="sListSize">'         .htmlEscape($add['listSize'])         .'</td>';
		echo '<td class="sListType">'         .htmlEscape($add['listType'])         .'</td>';

}

////////////////////////////////////////////////////////////////////////////////
//HTMLエスケープして引数を返す
////////////////////////////////////////////////////////////////////////////////
function htmlEscape($str){
	//'&'⇒'&amp;'
	//'"'⇒'&quot;'
	//'<'⇒'&lt;'
	//'>'⇒'&gt;'
	$str = htmlspecialchars($str,ENT_COMPAT ,'SJIS');
	$str = str_replace(" ", "&nbsp;", $str);
	return $str;
}

////////////////////////////////////////////////////////////////////////////////
//ランダムの文字列作成
////////////////////////////////////////////////////////////////////////////////
function makeRandStr($length) {
    $str = array_merge(range('a', 'z'), range('0', '9'), range('A', 'Z'));
    $r_str = null;
    for ($i = 0; $i < $length; $i++) {
        $r_str .= $str[rand(0, count($str)-1)];
    }
    return $r_str;
}

////////////////////////////////////////////////////////////////////////////////
//$sqlExec(文末のLIMIT文を取り除いたSQL）（※文末「;」無し）
////////////////////////////////////////////////////////////////////////////////
function getSqlExec($sql) {
	//LIMIT文は削除（不要のため）
	$pattern = '/LIMIT(.)*/';
	$replacement = '';
	$sqlExec = preg_replace($pattern, $replacement, $sql);

	//文末の「;」削除（上では「；」が削除されなかったため
	$pattern = '/;/';
	$replacement = '';
	$sqlExec = preg_replace($pattern, $replacement, $sqlExec);
	return $sqlExec;
}

////////////////////////////////////////////////////////////////////////////////
//$sqlCount(SQL文から項目名だけを取り出しもの）
////////////////////////////////////////////////////////////////////////////////
function getSqlDisps($sql) {
	//実行SQLより画面表示用の項目名の部分を取得
	$disps       = array();
	$pattern     = '/FROM(.)*/';
	$replacement = '';
	$sqlTmp = preg_replace($pattern, $replacement, $sql);
	$sqlTmps = explode(",", $sqlTmp);
	foreach($sqlTmps as $tmp){
		if (preg_match('/AS "/', $tmp)) {
			$disp   = array();
			$add = strstr($tmp, 'AS "');
			$pattern = '/AS /';
			$replacement = '';
			$add = preg_replace($pattern, $replacement, $add);
			$pattern = '/U /';
			$replacement = '';
			$add = preg_replace($pattern, $replacement, $add);
			$add = trim($add);
			$pattern = '/"/';
			$replacement = '';
			$add = preg_replace($pattern, $replacement, $add);
			$disp["dispname"] = $add;
			array_push($disps, $disp);
		}
	}
	return $disps;
}

////////////////////////////////////////////////////////////////////////////////
//変換すべき番号を返す
////////////////////////////////////////////////////////////////////////////////
function LeftConvInner($tableName,$joins){
	foreach($joins as $join){
		$tmps = explode(",", $join);
		if($tmps[0] == $tableName){
		}
		if($tmps[2] == $tableName){
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
//メッセージを画面上に表示する
////////////////////////////////////////////////////////////////////////////////
function echoMessage($code){
	global $MESSAGE;
	if($code != ""){
		echo $MESSAGE[$code];
	}
}

////////////////////////////////////////////////////////////////////////////////
//メッセージコードを追加していく
////////////////////////////////////////////////////////////////////////////////
function addCode(&$codes,$code){
	if($codes != ""){
		$codes = ','.$code;
	}else{
		$codes = $code;
	}
}

////////////////////////////////////////////////////////////////////////////////
//メッセージを画面上に表示する
////////////////////////////////////////////////////////////////////////////////
function echoMessages($codes){
	global $MESSAGE;
	$codesArray = explode(",", $codes);
	$loopCount = 0;
	foreach($codesArray as $code){
		if($loopCount != 0){
			echo '<br/>';
		}
		if($code != ""){
			echo $MESSAGE[$code];
		}
		$loopCount++;
	}
}

?>
