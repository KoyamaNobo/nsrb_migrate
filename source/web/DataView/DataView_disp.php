<?php
session_start();

////////////////////////////////////////////////////////////////////////////////
//共通ライブラリ読み込み
////////////////////////////////////////////////////////////////////////////////
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//実行時間無限大
////////////////////////////////////////////////////////////////////////////////
set_time_limit(0);

////////////////////////////////////////////////////////////////////////////////
//変数初期化
////////////////////////////////////////////////////////////////////////////////
$post                = $_POST;
$get                 = $_GET;
$PrePgName           = '';        //戻り先のURL
$selectedBonds       = array();
$selectedItemNames   = array();
$selectedFilters     = array();
$selectedSorts       = array();
$getDatas            = array();
$getDatasNext        = array();
$selectedCommandId   = '';
$selectedCommandName = '';
$sql                 = '';
$sqlCount            = '';
$Del_Flg             = '0';
$selectedTables      = array();
$filters             = array();
$sorts               = array();
$outputfile          = 'DV' . date("YmdHis") . makeRandStr(5) . '.csv';
$disps               = array();
$joins               = array();
$genzai_page         = '';
$NextPage            = array();
$today               = date("Y-m-d H:i:s");
$sqlSaveFlg          = false;  //実行するSQLを保存するかのフラグ
$Processing_Flg      = '';     //SQLを保存する場合の処理フラグ 1：自由検索  2：条件タグ検索
$successCode         = '';
$message_codes       = '';
$pageing_mes         = '';

////////////////////////////////////////////////////////////////////////////////
//DB接続（DataView用コネクション:$dbhDVの作成）
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB接続に失敗した場合
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//DB接続（日進用コネクション:$dbhNISの作成）
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//ログインチェック
////////////////////////////////////////////////////////////////////////////////
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//引数の取得
////////////////////////////////////////////////////////////////////////////////
if(!empty($post)){
	mb_regex_encoding('SJIS');
	//遷移元の画面名取得
	if(isset($post["pgName"]) && !empty($post["pgName"])){
		//前のページからの遷移のとき
		$prePgName = $post["pgName"];
	}else{
		//ページングで処理された時前のページは引き継いでおく
		$prePgName = $post["prePgName"];
	}

	//条件タグID取得
	if(isset($post['selectedCommandId'])){
		//条件タグ検索から遷移してきた場合
		$selectedCommandId = $post['selectedCommandId'];

	}else{
		//自由検索から遷移してきた場合
		//存在チェック＆指定されたテーブルの一覧取得
		if(!empty($post["selectedTables"])){
			foreach($post['selectedTables'] as $tmp){
				array_push($selectedTables,  trim($tmp));
			}
		}
		//結合条件取得
		if(!empty($post["selectedBonds"])){
			foreach($post["selectedBonds"] as $tmp){
				array_push($selectedBonds, $tmp);
			}
		}
		//表示項目取得
		if(!empty($post["selectedItemNames"])){
			foreach($post["selectedItemNames"] as $tmp){
				array_push($selectedItemNames, $tmp);
			}
		}
		//抽出条件取得
		if(!empty($post["selectedFilters"])){
			foreach($post["selectedFilters"] as $tmp){
				array_push($selectedFilters, $tmp);
			}
		}
		//並べ替え取得
		if(!empty($post["selectedSorts"])){
			foreach($post["selectedSorts"] as $tmp){
				array_push($selectedSorts, $tmp);
			}
		}
	}
}else{
	//URLのみで呼び出された時
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//実際実行するSQL条件タグ作成
////////////////////////////////////////////////////////////////////////////////
if(!empty($selectedCommandId)){
	$Processing_Flg = '2';
	//データベースより実効SQL文取得
	$sqlCommand = "SELECT Sql_str,Command_Name FROM t_command WHERE Command_Id=:Command_Id AND Del_Flg=:Del_Flg;";
	$sth = $dbhDV->prepare($sqlCommand);
	$sth->bindParam('Command_Id',$selectedCommandId);
	$sth->bindParam('Del_Flg',$Del_Flg);
	//SQL実行
	$sth->execute();
	while($data = $sth->fetch(PDO::FETCH_ASSOC)){
		$sql = $data['Sql_str'];
		$selectedCommandName = $data['Command_Name'];
	}

	//SQL文からLIMIT文を外したものを取得（文末の「;」も外したもの）
	$sql = getSqlExec($sql);

	//実行SQLより画面表示用の項目名の部分を取得
	$disps = getSqlDisps($sql);

}else{
	$Processing_Flg = '1';
	/////////////////////////////////////////////////////////////////////////////
	//SELECT部分作成
	/////////////////////////////////////////////////////////////////////////////
	//使いやすいようにパラメータ変換-------------------------------------------//
	foreach($selectedItemNames as $tmp){
		$tmps  = mb_split ('~', $tmp);
		$tmps2 = mb_split('.', $tmps[1]);
		$disp   = array();
		$disp["tablename"] = $tmps[0];               //テーブル名
		$disp["start"]     = $tmps[4];               //開始位置
		$disp["size"]      = $tmps[5];               //サイズ
		$disp["type"]      = $tmps[6];               //型
		$disp["cast"]      = "CHAR";                 //出力時の変換形式
		$disp["dispname"]  = $tmps[0].".".$tmps[1];  //出力時の表示名
		array_push($disps, $disp);
	}

	//SQL作成（SELECT部分）-------------------------------------------//
	$roopCount = 0;
	foreach($disps as $disp){
		$roopCount++;

		if($roopCount == 1){
			$sql .= " SELECT ";
		}else{
			$sql .= " , ";
		}

		if($disp["type"] == "S9"){
			//S9の場合
			$sql .= 'CASE mid(hex(MID( `'.$disp["tablename"].'`.item,'.$disp["start"].','.$disp["size"].' )),'.$disp["size"].'*2 -1, 1 ) ';
			$sql .= 'WHEN "7" THEN ';
			$sql .= 'CAST(CONCAT("-"';
			for ($i = 1; $i <= (int)$disp["size"]; $i++) {
				$sql .= ' , ';
				$sql .= ' MID(HEX(MID( `'.$disp["tablename"].'`.item,'.$disp["start"].','.$disp["size"].' )),'.$i.' * 2 , 1 ) ';
			}
			$sql .= ' ) AS CHAR ) ';
			$sql .= 'ELSE ';
			$sql .= 'CAST(CONCAT(""';
			for ($i = 1; $i <= (int)$disp["size"]; $i++) {
				$sql .= ' , ';
				$sql .= ' MID(HEX(MID( `'.$disp["tablename"].'`.item,'.$disp["start"].','.$disp["size"].' )),'.$i.' * 2 , 1 ) ';
			}
			$sql .= ' ) AS CHAR ) ';
			$sql .= ' END ';
			$sql .= ' AS "'. $disp["dispname"] .'" ' ;
		}else if($disp["type"] == "C9" || $disp["type"] == "SC9") {
			//C9,SC9の場合
			$sql .= ' CASE MID(HEX(MID(`'.$disp["tablename"].'`.item , '.$disp["start"].' , '.$disp["size"].' )),'.$disp["size"].' * 2 , 1 ) ';
			$sql .= ' WHEN "D" THEN  ';
			$sql .= ' CONCAT( "-", ';
			$sql .= ' MID(HEX(MID(`'.$disp["tablename"].'`.item , '.$disp["start"].' , '.$disp["size"].' )), 1 , '.$disp["size"].' * 2 - 1 ))';
			$sql .= ' ELSE ';
			$sql .= ' CONCAT( "", ';
			$sql .= ' MID(HEX(MID(`'.$disp["tablename"].'`.item , '.$disp["start"].' , '.$disp["size"].' )), 1 , '.$disp["size"].' * 2 - 1 ))';
			$sql .= ' END ';
			$sql .= ' AS "'. $disp["dispname"] .'" ' ;
		}else if($disp["type"] == "9" || $disp["type"] == "99" || $disp["type"] == "N" || $disp["type"] == "X" || $disp["type"] == "" ) {
			//9,99,N,X,空の場合
			if ($disp["cast"] != ""){
				$sql .= 'CAST(';
			}
			$sql .= 'MID( `'.$disp["tablename"].'`.item,'.$disp["start"].','.$disp["size"].' ) ';

			if ($disp["cast"] != ""){
				$sql .= ' AS '.$disp["cast"].' )';
			}
			$sql .= ' AS "'. $disp["dispname"] .'" ' ;
		} else {
			//URLのみで呼び出された時
			addCode($message_codes,'ERRJ0061');
			put_error('ERRJ0061','');
		}
	}

shell_exec('logger -i "^sql^'.mb_convert_encoding($sql,'UTF-8','auto').'"');
	/////////////////////////////////////////////////////////////////////////////
	//JOIN部分作成
	/////////////////////////////////////////////////////////////////////////////
	//使いやすいようにパラメータ変換-------------------------------------------//
	foreach($selectedBonds as $tmp){
		$join     = array();
		//string(54) "AHNHF.AHNH-STC~和名~1~7~9~LEFTJOIN~B-TCM.AHNH-R~~1~64~"
		$tmps = explode("~", $tmp);
		$ltmps = explode(".", $tmps[0]);
		$rtmps = explode(".", $tmps[6]);

		$join["ltname"]  = $ltmps[0];
		$join["ltstart"] = $tmps[2];
		$join["ltsize"]  = $tmps[3];
		$join["lttype"]  = $tmps[4];
		$join["ltcast"] = "CHAR";     //結合させる時は型が何であってもchar型変換してから
		//同じテーブルの時ANDを考慮するならここか？ comment koyama
		if($tmps[5]=='LEFTJOIN'){
			$join["how"]     = 'LEFT JOIN';
		}else{
			$join["how"]     = 'INNER JOIN';
		}

		$join["rtname"]  = $rtmps[0];
		$join["rtstart"] = $tmps[8];
		$join["rtsize"]  = $tmps[9];
		$join["rttype"]  = $tmps[10];
		$join["rtcast"] = "CHAR";    //結合させる時は型が何であってもchar型変換してから

		array_push($joins, $join);
	}
	//SQL作成（JOIN部分）-------------------------------------------//
	$roopCount = 0;
	foreach($joins as $join){
		$roopCount++;
		if($roopCount == 1){
			$sql      .= " FROM `" . $join["ltname"] . "` ";
		}
		$sql      .= ' '.$join["how"].' `'.$join["rtname"].'` ON ';
		if ($join["ltcast"] != ""){
			$sql      .= ' CAST(';
		}
		$sql      .= ' MID( `'.$join["ltname"].'`.item,'.$join["ltstart"].','.$join["ltsize"].' ) ';
		if ($join["ltcast"] != ""){
			$sql      .= ' AS '.$join["ltcast"].' ) ';
		}
		$sql      .= ' = ';

		if ($join["rtcast"] != ""){
			$sql      .= 'CAST(';
		}
		$sql      .= 'MID( `'.$join["rtname"].'`.item,'.$join["rtstart"].','.$join["rtsize"].' ) ';
		if ($join["rtcast"] != ""){
			$sql      .= ' AS '.$join["rtcast"].' ) ';
		}
	}
	/////////////////////////////////////////////////////////////////////////////
	//WHERE部分作成
	/////////////////////////////////////////////////////////////////////////////
	//使いやすいようにパラメータ変換-------------------------------------------//
	foreach($selectedFilters as $tmp){
		//and~TM1~JNSR-04~12~4~C9~=~0033~JNSR.JNSR-04

		$tmps = explode("~", $tmp);
		$filter["how"]      = $tmps[0];  //AND or OR
		$filter["tname"]    = $tmps[1];  //テーブル名
		$filter["tstart"]   = $tmps[4];  //開始位置
		$filter["tsize"]    = $tmps[5];  //サイズ
		$filter["ttype"]    = $tmps[6];  //型
		$filter["operator"] = $tmps[7];  //比較演算子
		$filter["value"]    = $tmps[8];  //値
		$filter["chValue"]  = '';

		switch ($filter["ttype"]) {
			case "9":
			case "99":
			case "C9":
			case "S9":
			case "SC9":
				$filter["cast"] = "SIGNED";//変換形式
				break;
			case "N":
			case "X":
			default:
				$filter["cast"] = "CHAR";//変換形式
				break;
		}
		//検索値がまナイスか判定
		if (preg_match("/^-/", $filter["value"])) {
			$filter["valueMinus"] = true;
		} else {
			$filter["valueMinus"] = false;
		}

		array_push($filters, $filter);
	}
	//SQL作成（WHERE部分）-------------------------------------------//
	//$sql .= ' WHERE mid(hex(MID( `JNSR1`.item ,1,4 )) , 8, 1 ) = '."'D'".' ';
	$roopCount = 0;
	foreach($filters as $filter){
		$roopCount++;
		if($roopCount == 1){
			$sql      .= " WHERE ";
		}else{
			$sql      .= " ". $filter["how"] ." ";
		}

		//タイプ別にvalue値を変換
		switch ($filter["ttype"]) {
			case "9":
			case "99":
			case "C9":
			case "S9":
			case "SC9":
 				$filter["chValue"] = (int)$filter["value"];
				break;
			case "N": //日本語検索は使用しない
			case "X":
			default:
				//右側をスペースで埋める
				$tmp = $filter["value"];
				$filter["chValue"] = str_pad($filter["value"], (int)$filter["tsize"] , " ", STR_PAD_RIGHT);
				break;
		}
		switch ($filter["ttype"]) {
			case "C9":
			case "SC9":
				$sql .= ' CASE MID(HEX(MID(`'.$filter["tname"].'`.item , '.$filter["tstart"].' , '.$filter["tsize"].' )),'.$filter["tsize"].' * 2 , 1 ) ';
				$sql .= ' WHEN "D" THEN  ';
				$sql .= ' CAST(CONCAT( "-", ';
				$sql .= ' MID(HEX(MID(`'.$filter["tname"].'`.item , '.$filter["tstart"].' , '.$filter["tsize"].' )), 1 , '.$filter["tsize"].' * 2 - 1 )) AS '. $filter["cast"] .') ';
				$sql .= ' ELSE ';
				$sql .= ' CAST(CONCAT( "", ';
				$sql .= ' MID(HEX(MID(`'.$filter["tname"].'`.item , '.$filter["tstart"].' , '.$filter["tsize"].' )), 1 , '.$filter["tsize"].' * 2 - 1 )) AS '. $filter["cast"] .') ';
				$sql .= ' END ';
				$sql .= ' '. $filter["operator"] ." CAST('".$filter["chValue"]."' AS " .$filter["cast"]. ') ';
				break;
			case "S9":
				$sql .= 'CASE mid(hex(MID( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' )),'.$filter["tsize"].'*2 -1, 1 ) ';
				$sql .= 'WHEN "7" THEN ';
				$sql .= 'CAST(CONCAT("-"';
				for ($i = 1; $i <= (int)$filter["tsize"]; $i++) {
					$sql .= ' , ';
					$sql .= ' MID(HEX(MID( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' )),'.$i.' * 2 , 1 ) ';
				}
				$sql .= ' ) AS ' .$filter["cast"]. ') ';
				$sql .= 'ELSE ';
				$sql .= 'CAST(CONCAT(""';
				for ($i = 1; $i <= (int)$filter["tsize"]; $i++) {
					$sql .= ' , ';
					$sql .= ' MID(HEX(MID( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' )),'.$i.' * 2 , 1 ) ';
				}
				$sql .= ' ) AS ' .$filter["cast"]. ') ';
				$sql .= ' END ';
				$sql .= ' '. $filter["operator"] ." CAST('".$filter["chValue"]."' AS " .$filter["cast"]. ') ';
				break;
			case "9":
				$sql .= ' CAST(MID( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' ) AS '. $filter["cast"] .') '. $filter["operator"] ." CAST('".$filter["chValue"]."' AS " .$filter["cast"]. ') ';
				break;
			case "N"://日本語でのソートは行わない
			case "X":
			default:
				$sql .= ' HEX(chCharMap( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' )) '. $filter["operator"] ." HEX(chCharMap('".$filter["chValue"]."')) ";
				break;
		}
	}
	/////////////////////////////////////////////////////////////////////////////
	//ORDER BY部分作成
	/////////////////////////////////////////////////////////////////////////////
	//使いやすいように値格納---------------------------------------------------//
	foreach($selectedSorts as $tmp){
		//AHNHF~AHNH-TCD~asc~47~4~9~AHNHF
		//AHNHF~AHNH-CTC~desc~降順~40~7~AHNHF
		$tmps = explode("~", $tmp);
		$sort["tname"]    = $tmps[0];
		$sort["iname"]    = $tmps[1];
		$sort["how"]      = $tmps[3];
		$sort["start"]    = $tmps[4];
		$sort["size"]     = $tmps[5];
		$sort["type"]     = $tmps[6];

		switch ($sort["type"]) {
			case "C9":
			case "SC9":
			case "9":
			case "99":
			case "S9":
				$sort["cast"] = "SIGNED";
				break;
			case "N":
			case "X":
			default:
				$sort["cast"] = "CHAR";
				break;
		}
		array_push($sorts, $sort);
	}
	//SQL作成（ORDER BY部分）----------------------------------------------------//
	$roopCount = 0;
	foreach($sorts as $sort){
		$roopCount++;
		if($roopCount == 1){
			$sql      .= " ORDER BY ";
		}else{
			$sql      .= " , ";
		}
		switch ($sort["type"]) {
			case "C9":
			case "SC9":
				$sql .= ' CASE MID(HEX(MID(`'.$sort["tname"].'`.item , '.$sort["start"].' , '.$sort["size"].' )),'.$sort["size"].' * 2 , 1 ) ';
				$sql .= ' WHEN "D" THEN  ';
				$sql .= ' CAST(CONCAT( "-", ';
				$sql .= ' MID(HEX(MID(`'.$sort["tname"].'`.item , '.$sort["start"].' , '.$sort["size"].' )), 1 , '.$sort["size"].' * 2 - 1 )) AS '. $sort["cast"] .') ';
				$sql .= ' ELSE ';
				$sql .= ' CAST(CONCAT( "", ';
				$sql .= ' MID(HEX(MID(`'.$sort["tname"].'`.item , '.$sort["start"].' , '.$sort["size"].' )), 1 , '.$sort["size"].' * 2 - 1 )) AS '. $sort["cast"] .') ';
				$sql .= ' END ';
				break;
			case "S9":
				$sql .= 'CASE mid(hex(MID( `'.$sort["tname"].'`.item,'.$sort["start"].','.$sort["size"].' )),'.$sort["size"].'*2 -1, 1 ) ';
				$sql .= 'WHEN "7" THEN ';
				$sql .= 'CAST(CONCAT("-"';
				for ($i = 1; $i <= (int)$sort["size"]; $i++) {
					$sql .= ' , ';
					$sql .= ' MID(HEX(MID( `'.$sort["tname"].'`.item,'.$sort["start"].','.$sort["size"].' )),'.$i.' * 2 , 1 ) ';
				}
				$sql .= ' ) AS ' .$sort["cast"]. ') ';
				$sql .= 'ELSE ';
				$sql .= 'CAST(CONCAT(""';
				for ($i = 1; $i <= (int)$sort["size"]; $i++) {
					$sql .= ' , ';
					$sql .= ' MID(HEX(MID( `'.$sort["tname"].'`.item,'.$sort["start"].','.$sort["size"].' )),'.$i.' * 2 , 1 ) ';
				}
				$sql .= ' ) AS ' .$sort["cast"]. ') ';
				$sql .= ' END ';
				break;
			case "9":
				$sql .= ' CAST(MID( `'.$sort["tname"].'`.item,'.$sort["start"].','.$sort["size"].' ) AS '. $sort["cast"] .') ';
				break;
			case "N":
			case "X":
			default:
				$sql .= ' HEX(chCharMap( `'.$sort["tname"].'`.item,'.$sort["start"].','.$sort["size"].' )) ';
				break;
		}
		$sql .= ' '.$sort["how"].' ';
	}
}


//ページングのための処理
$genzai_page = 1;
$start = 0;//このスタートは配列の中の何個目から取り出すのか？
$sqlSaveFlg = true;
$hyouji_kazu = (int)$_SESSION['dv_user_disp_num'];//表示する数

//ページのリンクの場合
if(isset($get["page"]) && $get['page'] <> ""){
	$genzai_page = $get['page'];//$getでもらった数字が現在のページ
	$start      = ($genzai_page-1)*$hyouji_kazu;//表示スタート数（何個目から表示するか？）
	$sqlSaveFlg = false;
}

//初回検索時は履歴テーブルへSQL保存
if($sqlSaveFlg){
	$sqlHis  = " INSERT INTO t_history (User_Id,Sqlstr,Exec_Date,Processing_Flg,Cre_Date,Mod_Date,Del_Flg ) ";
	$sqlHis .= " VALUES                (:User_Id,:Sqlstr,:Exec_Date,:Processing_Flg,:Cre_Date,:Mod_Date,:Del_Flg ) ;";
	$sqlH = $sql.';';
	$sth = $dbhDV->prepare($sqlHis);
	$sth->bindParam('User_Id',$_SESSION['dv_user_id']);
	$sth->bindParam('Sqlstr',$sqlH);
	$sth->bindParam('Exec_Date',$today);
	$sth->bindParam('Processing_Flg',$Processing_Flg);
	$sth->bindParam('Cre_Date',$today);
	$sth->bindParam('Mod_Date',$today);
	$sth->bindParam('Del_Flg',$Del_Flg);
	//SQL実行
	$sth->execute();
}
////////////////////////////////////////////////////////////////////////////////
//データ取得
////////////////////////////////////////////////////////////////////////////////
//SQL文にリミット追加
$sqlA = $sql. ' LIMIT '.$start. "," .$hyouji_kazu.";";
//SQL実行
$sth = $dbhNIS->prepare($sqlA);
$sth->execute();
while($data = $sth->fetch(PDO::FETCH_ASSOC)){
	$add     = array();
	foreach($disps as $disp){
		$add[$disp["dispname"]] = $data[$disp["dispname"]];
	}
	array_push($getDatas, $add);
}

////////////////////////////////////////////////////////////////////////////////
//データ取得（次のページ以降のデータが存在しているか）
////////////////////////////////////////////////////////////////////////////////
//SQL文にリミット追加
$nextStart = $start;
for ($i = 1; $i <= PAGINUM; $i++) {
	$getDatasNext= array();
    $nextStart = $nextStart + $hyouji_kazu;

	$sqlB = $sql. ' LIMIT '.$nextStart. "," .$hyouji_kazu.";";
	$sth = $dbhNIS->prepare($sqlB);
	$sth->execute();
	while($data = $sth->fetch(PDO::FETCH_ASSOC)){
		$add     = array();
		foreach($disps as $disp){
			$add[$disp["dispname"]] = $data[$disp["dispname"]];
		}
		array_push($getDatasNext, $add);
	}
	if(count($getDatasNext)>0){
		array_push($NextPage, $genzai_page + $i);
	}
}


//画面描画
require_once('./view/vw_DataView_disp.php');

?>
