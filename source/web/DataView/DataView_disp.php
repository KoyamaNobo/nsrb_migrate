<?php
session_start();

////////////////////////////////////////////////////////////////////////////////
//���ʃ��C�u�����ǂݍ���
////////////////////////////////////////////////////////////////////////////////
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//���s���Ԗ�����
////////////////////////////////////////////////////////////////////////////////
set_time_limit(0);

////////////////////////////////////////////////////////////////////////////////
//�ϐ�������
////////////////////////////////////////////////////////////////////////////////
$post                = $_POST;
$get                 = $_GET;
$PrePgName           = '';        //�߂���URL
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
$sqlSaveFlg          = false;  //���s����SQL��ۑ����邩�̃t���O
$Processing_Flg      = '';     //SQL��ۑ�����ꍇ�̏����t���O 1�F���R����  2�F�����^�O����
$successCode         = '';
$message_codes       = '';
$pageing_mes         = '';

////////////////////////////////////////////////////////////////////////////////
//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
////////////////////////////////////////////////////////////////////////////////
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//���O�C���`�F�b�N
////////////////////////////////////////////////////////////////////////////////
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//�����̎擾
////////////////////////////////////////////////////////////////////////////////
if(!empty($post)){
	mb_regex_encoding('SJIS');
	//�J�ڌ��̉�ʖ��擾
	if(isset($post["pgName"]) && !empty($post["pgName"])){
		//�O�̃y�[�W����̑J�ڂ̂Ƃ�
		$prePgName = $post["pgName"];
	}else{
		//�y�[�W���O�ŏ������ꂽ���O�̃y�[�W�͈����p���ł���
		$prePgName = $post["prePgName"];
	}

	//�����^�OID�擾
	if(isset($post['selectedCommandId'])){
		//�����^�O��������J�ڂ��Ă����ꍇ
		$selectedCommandId = $post['selectedCommandId'];

	}else{
		//���R��������J�ڂ��Ă����ꍇ
		//���݃`�F�b�N���w�肳�ꂽ�e�[�u���̈ꗗ�擾
		if(!empty($post["selectedTables"])){
			foreach($post['selectedTables'] as $tmp){
				array_push($selectedTables,  trim($tmp));
			}
		}
		//���������擾
		if(!empty($post["selectedBonds"])){
			foreach($post["selectedBonds"] as $tmp){
				array_push($selectedBonds, $tmp);
			}
		}
		//�\�����ڎ擾
		if(!empty($post["selectedItemNames"])){
			foreach($post["selectedItemNames"] as $tmp){
				array_push($selectedItemNames, $tmp);
			}
		}
		//���o�����擾
		if(!empty($post["selectedFilters"])){
			foreach($post["selectedFilters"] as $tmp){
				array_push($selectedFilters, $tmp);
			}
		}
		//���בւ��擾
		if(!empty($post["selectedSorts"])){
			foreach($post["selectedSorts"] as $tmp){
				array_push($selectedSorts, $tmp);
			}
		}
	}
}else{
	//URL�݂̂ŌĂяo���ꂽ��
	addCode($message_codes,'ERR0501');
	put_error('ERR0501','');
	header('Location: ./DataView_index.php?message='.urlencode($message_codes).'');
}

////////////////////////////////////////////////////////////////////////////////
//���ێ��s����SQL�����^�O�쐬
////////////////////////////////////////////////////////////////////////////////
if(!empty($selectedCommandId)){
	$Processing_Flg = '2';
	//�f�[�^�x�[�X������SQL���擾
	$sqlCommand = "SELECT Sql_str,Command_Name FROM t_command WHERE Command_Id=:Command_Id AND Del_Flg=:Del_Flg;";
	$sth = $dbhDV->prepare($sqlCommand);
	$sth->bindParam('Command_Id',$selectedCommandId);
	$sth->bindParam('Del_Flg',$Del_Flg);
	//SQL���s
	$sth->execute();
	while($data = $sth->fetch(PDO::FETCH_ASSOC)){
		$sql = $data['Sql_str'];
		$selectedCommandName = $data['Command_Name'];
	}

	//SQL������LIMIT�����O�������̂��擾�i�����́u;�v���O�������́j
	$sql = getSqlExec($sql);

	//���sSQL����ʕ\���p�̍��ږ��̕������擾
	$disps = getSqlDisps($sql);

}else{
	$Processing_Flg = '1';
	/////////////////////////////////////////////////////////////////////////////
	//SELECT�����쐬
	/////////////////////////////////////////////////////////////////////////////
	//�g���₷���悤�Ƀp�����[�^�ϊ�-------------------------------------------//
	foreach($selectedItemNames as $tmp){
		$tmps  = mb_split ('~', $tmp);
		$tmps2 = mb_split('.', $tmps[1]);
		$disp   = array();
		$disp["tablename"] = $tmps[0];               //�e�[�u����
		$disp["start"]     = $tmps[4];               //�J�n�ʒu
		$disp["size"]      = $tmps[5];               //�T�C�Y
		$disp["type"]      = $tmps[6];               //�^
		$disp["cast"]      = "CHAR";                 //�o�͎��̕ϊ��`��
		$disp["dispname"]  = $tmps[0].".".$tmps[1];  //�o�͎��̕\����
		array_push($disps, $disp);
	}

	//SQL�쐬�iSELECT�����j-------------------------------------------//
	$roopCount = 0;
	foreach($disps as $disp){
		$roopCount++;

		if($roopCount == 1){
			$sql .= " SELECT ";
		}else{
			$sql .= " , ";
		}

		if($disp["type"] == "S9"){
			//S9�̏ꍇ
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
			//C9,SC9�̏ꍇ
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
			//9,99,N,X,��̏ꍇ
			if ($disp["cast"] != ""){
				$sql .= 'CAST(';
			}
			$sql .= 'MID( `'.$disp["tablename"].'`.item,'.$disp["start"].','.$disp["size"].' ) ';

			if ($disp["cast"] != ""){
				$sql .= ' AS '.$disp["cast"].' )';
			}
			$sql .= ' AS "'. $disp["dispname"] .'" ' ;
		} else {
			//URL�݂̂ŌĂяo���ꂽ��
			addCode($message_codes,'ERRJ0061');
			put_error('ERRJ0061','');
		}
	}

shell_exec('logger -i "^sql^'.mb_convert_encoding($sql,'UTF-8','auto').'"');
	/////////////////////////////////////////////////////////////////////////////
	//JOIN�����쐬
	/////////////////////////////////////////////////////////////////////////////
	//�g���₷���悤�Ƀp�����[�^�ϊ�-------------------------------------------//
	foreach($selectedBonds as $tmp){
		$join     = array();
		//string(54) "AHNHF.AHNH-STC~�a��~1~7~9~LEFTJOIN~B-TCM.AHNH-R~~1~64~"
		$tmps = explode("~", $tmp);
		$ltmps = explode(".", $tmps[0]);
		$rtmps = explode(".", $tmps[6]);

		$join["ltname"]  = $ltmps[0];
		$join["ltstart"] = $tmps[2];
		$join["ltsize"]  = $tmps[3];
		$join["lttype"]  = $tmps[4];
		$join["ltcast"] = "CHAR";     //���������鎞�͌^�����ł����Ă�char�^�ϊ����Ă���
		//�����e�[�u���̎�AND���l������Ȃ炱�����H comment koyama
		if($tmps[5]=='LEFTJOIN'){
			$join["how"]     = 'LEFT JOIN';
		}else{
			$join["how"]     = 'INNER JOIN';
		}

		$join["rtname"]  = $rtmps[0];
		$join["rtstart"] = $tmps[8];
		$join["rtsize"]  = $tmps[9];
		$join["rttype"]  = $tmps[10];
		$join["rtcast"] = "CHAR";    //���������鎞�͌^�����ł����Ă�char�^�ϊ����Ă���

		array_push($joins, $join);
	}
	//SQL�쐬�iJOIN�����j-------------------------------------------//
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
	//WHERE�����쐬
	/////////////////////////////////////////////////////////////////////////////
	//�g���₷���悤�Ƀp�����[�^�ϊ�-------------------------------------------//
	foreach($selectedFilters as $tmp){
		//and~TM1~JNSR-04~12~4~C9~=~0033~JNSR.JNSR-04

		$tmps = explode("~", $tmp);
		$filter["how"]      = $tmps[0];  //AND or OR
		$filter["tname"]    = $tmps[1];  //�e�[�u����
		$filter["tstart"]   = $tmps[4];  //�J�n�ʒu
		$filter["tsize"]    = $tmps[5];  //�T�C�Y
		$filter["ttype"]    = $tmps[6];  //�^
		$filter["operator"] = $tmps[7];  //��r���Z�q
		$filter["value"]    = $tmps[8];  //�l
		$filter["chValue"]  = '';

		switch ($filter["ttype"]) {
			case "9":
			case "99":
			case "C9":
			case "S9":
			case "SC9":
				$filter["cast"] = "SIGNED";//�ϊ��`��
				break;
			case "N":
			case "X":
			default:
				$filter["cast"] = "CHAR";//�ϊ��`��
				break;
		}
		//�����l���܃i�C�X������
		if (preg_match("/^-/", $filter["value"])) {
			$filter["valueMinus"] = true;
		} else {
			$filter["valueMinus"] = false;
		}

		array_push($filters, $filter);
	}
	//SQL�쐬�iWHERE�����j-------------------------------------------//
	//$sql .= ' WHERE mid(hex(MID( `JNSR1`.item ,1,4 )) , 8, 1 ) = '."'D'".' ';
	$roopCount = 0;
	foreach($filters as $filter){
		$roopCount++;
		if($roopCount == 1){
			$sql      .= " WHERE ";
		}else{
			$sql      .= " ". $filter["how"] ." ";
		}

		//�^�C�v�ʂ�value�l��ϊ�
		switch ($filter["ttype"]) {
			case "9":
			case "99":
			case "C9":
			case "S9":
			case "SC9":
 				$filter["chValue"] = (int)$filter["value"];
				break;
			case "N": //���{�ꌟ���͎g�p���Ȃ�
			case "X":
			default:
				//�E�����X�y�[�X�Ŗ��߂�
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
			case "N"://���{��ł̃\�[�g�͍s��Ȃ�
			case "X":
			default:
				$sql .= ' HEX(chCharMap( `'.$filter["tname"].'`.item,'.$filter["tstart"].','.$filter["tsize"].' )) '. $filter["operator"] ." HEX(chCharMap('".$filter["chValue"]."')) ";
				break;
		}
	}
	/////////////////////////////////////////////////////////////////////////////
	//ORDER BY�����쐬
	/////////////////////////////////////////////////////////////////////////////
	//�g���₷���悤�ɒl�i�[---------------------------------------------------//
	foreach($selectedSorts as $tmp){
		//AHNHF~AHNH-TCD~asc~47~4~9~AHNHF
		//AHNHF~AHNH-CTC~desc~�~��~40~7~AHNHF
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
	//SQL�쐬�iORDER BY�����j----------------------------------------------------//
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


//�y�[�W���O�̂��߂̏���
$genzai_page = 1;
$start = 0;//���̃X�^�[�g�͔z��̒��̉��ڂ�����o���̂��H
$sqlSaveFlg = true;
$hyouji_kazu = (int)$_SESSION['dv_user_disp_num'];//�\�����鐔

//�y�[�W�̃����N�̏ꍇ
if(isset($get["page"]) && $get['page'] <> ""){
	$genzai_page = $get['page'];//$get�ł���������������݂̃y�[�W
	$start      = ($genzai_page-1)*$hyouji_kazu;//�\���X�^�[�g���i���ڂ���\�����邩�H�j
	$sqlSaveFlg = false;
}

//���񌟍����͗����e�[�u����SQL�ۑ�
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
	//SQL���s
	$sth->execute();
}
////////////////////////////////////////////////////////////////////////////////
//�f�[�^�擾
////////////////////////////////////////////////////////////////////////////////
//SQL���Ƀ��~�b�g�ǉ�
$sqlA = $sql. ' LIMIT '.$start. "," .$hyouji_kazu.";";
//SQL���s
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
//�f�[�^�擾�i���̃y�[�W�ȍ~�̃f�[�^�����݂��Ă��邩�j
////////////////////////////////////////////////////////////////////////////////
//SQL���Ƀ��~�b�g�ǉ�
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


//��ʕ`��
require_once('./view/vw_DataView_disp.php');

?>
