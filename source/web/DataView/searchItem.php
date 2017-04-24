<?php
session_start();
//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');
//////////////////////////////////////////////////////////////////////////////////////////////////function
////////////////////////////////////////////////////////////////////////////////
//M_ITEMELEMENT�̈ꗗ�擾
////////////////////////////////////////////////////////////////////////////////
function get_itemelements($tablename,$startNum,&$itemelements,&$message_codes,&$dbhNIS){
	$result = true;
	$roopCount = 0;
	//�e�[�u�����́u-�v���폜 upd koyama 20150924
	$TableNameLikeSplit = preg_replace('/-/',"", $tablename);

	//M_ITEMELEMENT���ގ�����
	$sql  = ' SELECT DISTINCT ';
	$sql .= '     SUBSTRING_INDEX(x.TableName, "_", 1) AS TN ';
	$sql .= '     ,levenshtein(SUBSTRING_INDEX(REPLACE(x.TableName,"-",""), "_", 1),:TableName) as lev ';
	$sql .= '     ,x.Label ';
	$sql .= '     ,x.S_Point ';
	$sql .= '     ,x.Size ';
	$sql .= '     ,x.DataType ';
	$sql .= '     ,y.Japanese_Name ';
/*	$sql .= '     ,y.NonDisp_Flg ';
	$sql .= '     ,y.Del_Flg ';*/
	$sql .= ' FROM ';
	$sql .= '     '.NIS_DB_NAME.'.M_ITEMELEMENT AS x ';
	$sql .= '     LEFT JOIN '.DB_NAME.'.m_japanese_name AS y ';
	$sql .= '         ON ';
	$sql .= '             SUBSTRING_INDEX(x.TableName, "_", 1) = y.TableName ';
	$sql .= '             AND x.Label     = y.Label ';
	$sql .= '             AND x.S_Point   = y.S_Point ';
	$sql .= '             AND x.Size      = y.Size ';
	$sql .= '             AND x.DataType  = y.DataType ';
	$sql .= '             AND y.Del_Flg   = 0 ';
	$sql .= ' WHERE ';
	$sql .= "     (y.NonDisp_Flg != '1' OR y.NonDisp_Flg is NULL) ";
	$sql .= ' ORDER BY ';
	$sql .= '     lev ASC ,';
	$sql .= '     TN ,';
	$sql .= '     x.S_Point ,';
	$sql .= '     x.Size ';
	$sql .= ' LIMIT '.$startNum. ' , '. GETITEMNUM .' ; ';
	$sth = $dbhNIS->prepare($sql);
	$sth->bindParam('TableName',$TableNameLikeSplit);
	// shell_exec('logger -i "^^^^^'.'"');
 	//SQL���s
	$sth->execute();
	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){
		$item = array();
		$item["tl"]             = $tmp['TN'] . "." . $tmp['Label'];
		$item["TableName"]      = $tmp['TN'];
		$item["Label"]          = $tmp['Label'];
		$item["S_Point"]        = $tmp['S_Point'];
		$item["Size"]           = $tmp['Size'];
		$item["DataType"]       = $tmp['DataType'];

		if(empty($tmp['Japanese_Name']) || $tmp['Japanese_Name']==""){
			$item["Japanese_Name"]  = " ";
		}else{
			$item["Japanese_Name"]  = $tmp['Japanese_Name'];
		}

		array_push($itemelements, $item);
		$roopCount++;
	}
	if($roopCount == 0){
		//���ڂ��P�������݂��Ă��Ȃ��Ƃ�
		add($message_codes,'ERR0004');
		put_error($message_codes,"");
		$result = false;
	}
	return $result;
}
////////////////////////////////////////////////////////////////////////////////
//M_ITEMELEMENT�̈ꗗ�擾(union)
////////////////////////////////////////////////////////////////////////////////
function get_itemelements_union($tablename,$startNum,&$itemelements,&$message_codes,&$dbhNIS,$srcFile){
	$result = true;
	$roopCount = 0;
	//�e�[�u�����́u-�v���폜 upd koyama 20150924
	$TableNameLikeSplit = preg_replace('/-/',"", $tablename);

	//M_ITEMELEMENT���ގ�����
	$sql .= "";
	$sql .= "SELECT DISTINCT ";
	$sql .= " SUBSTRING_INDEX(m.TableName, '_', 1) as TN ";
	$sql .= " ,Label as Label , S_Point as S_Point ,Size as Size ";
	$sql .= " ,DataType as DataType , Japanese_Name as Japanese_Name  ";
	$sql .= "FROM ";
	$sql .= "((SELECT DISTINCT      ";
	$sql .= "   x.TableName      as TableName        ";
	$sql .= ",  levenshtein(SUBSTRING_INDEX(REPLACE(x.TableName,'-',''), '_', 1),:TableName) as lev        ";
	$sql .= ",  x.Label          as Label            ";
	$sql .= ",  x.S_Point        as S_Point          ";
	$sql .= ",  x.Size           as Size             ";
	$sql .= ",  x.DataType       as DataType         ";
	$sql .= ",  y.Japanese_Name as Japanese_Name     ";
	$sql .= ",  y.NonDisp_Flg   as NonDisp_Flg       ";
	$sql .= ",  y.Del_Flg       as Del_Flg           ".PHP_EOL;
	$sql .= "FROM      ".NIS_DB_NAME.".M_ITEMELEMENT AS x       ";
	$sql .= "LEFT JOIN ".DB_NAME.".m_japanese_name AS y   ";
	$sql .= "ON              SUBSTRING_INDEX(x.TableName, '_', 1) = y.TableName ";
	$sql .= "AND              x.Label     = y.Label     ";
	$sql .= "AND              x.S_Point   = y.S_Point   ";
	$sql .= "AND              x.Size      = y.Size      ";
	$sql .= "AND              x.DataType  = y.DataType  ";
	$sql .= "WHERE      y.NonDisp_Flg != '1' OR y.NonDisp_Flg is NULL   ) ".PHP_EOL;
	$sql .= "UNION                                      ".PHP_EOL;
	$sql .= "(SELECT                                    ";
	$sql .= " z.TableName                 as TableName   ";
	$sql .= ",levenshtein(SUBSTRING_INDEX(REPLACE(z.TableName,'-',''), '_', 1),:TableName) as lev        ";
	$sql .= ",Item_Japanese_Name         as Label       ";
	$sql .= ",z.S_Point                  as S_Point     ";
	$sql .= ",z.Size                     as Size        ";
	$sql .= ",z.Data_Type                as Data_Type   ";
	$sql .= ",'���[�U��`'             as Japanese_Name ";
	$sql .= ",z.Priority_Disp_Flg        as NonDisp_Flg ";
	$sql .= ",z.Del_Flg                  as Del_Flg     ".PHP_EOL;
	$sql .= "FROM dataview.m_extension as  z            ";
	$sql .= "WHERE NOT(z.Priority_Disp_Flg = 0          ";
	$sql .= "AND z.User_Id <> :User_Id ) ";
	$sql .= "AND z.Del_Flg = 0 )) as m  ";
	$sql .= "ORDER BY  ";
	$sql .= " levenshtein(SUBSTRING_INDEX(REPLACE(m.TableName,'-',''), '_', 1),:TableName) ASC ";
	$sql .= ", SUBSTRING_INDEX(m.TableName, '_', 1) ,   m.S_Point ,     m.Size ";
	$sql .= ' LIMIT '.$startNum. ' , '. GETITEMNUM .' ; ';
	$sth = $dbhNIS->prepare($sql);
	$sth->bindParam('TableName',$TableNameLikeSplit);
	$sth->bindParam('User_Id'  ,$TableNameLikeSplit);
	//SQL���s
	$sth->execute();

	while($tmp = $sth->fetch(PDO::FETCH_ASSOC)){
		$item = array();
		$item["tl"]             = $tmp['TN'] . "." . $tmp['Label'];
		$item["TableName"]      = $tmp['TN'];
		$item["Label"]          = $tmp['Label'];
		$item["S_Point"]        = $tmp['S_Point'];
		$item["Size"]           = $tmp['Size'];
		$item["DataType"]       = $tmp['DataType'];
		if(empty($tmp['Japanese_Name']) || $tmp['Japanese_Name']==""){
			$item["Japanese_Name"]  = " ";
		}else{
			$item["Japanese_Name"]  = $tmp['Japanese_Name'];
		}
		array_push($itemelements, $item);
		$roopCount++;
	}
	if($roopCount == 0){
		//���ڂ��P�������݂��Ă��Ȃ��Ƃ�
		add($message_codes,'ERR0004');
		put_error($message_codes,"");
		$result = false;
	}
	return $result;
}
//////////////////////////////////////////////////////////////////////////////////////////////////function
//�ϐ�������
$post         = $_POST;
$itemelements = array();
$url          = basename($_SERVER['PHP_SELF'],".php");
$message_codes = 'ERR0601';

//DB�ڑ��iDataView�p�R�l�N�V����:$dbhDV�̍쐬�j
if(!db_connect(DB_NAME,DB_HOST,DB_PORT,DB_USER,DB_PASS,$dbhDV,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	echo $message_codes."\r\n";
	exit;
}

//DB�ڑ��i���i�p�R�l�N�V����:$dbhNIS�̍쐬�j
if(!db_connect(NIS_DB_NAME,NIS_DB_HOST,NIS_DB_PORT,NIS_DB_USER,NIS_DB_PASS,$dbhNIS,$message_codes)){
	//DB�ڑ��Ɏ��s�����ꍇ
	echo $message_codes."\r\n";
	exit;
}

//���O�C���`�F�b�N
if(!loginCheck($_SESSION['dv_user_id'], $_SESSION['dv_user_password'] ,$message_codes,$dbhDV)){
	echo $message_codes."\r\n";
	exit;
}

if(empty($post)){
	//post�p�����[�^�����݂��Ȃ��Ƃ�
	echo "ERR0601\r\n";
	put_error('ERR0601','');
	exit;
}

//���ڂ��擾
if(array_key_exists('srcUrl',$post) && preg_match('/free_search_3/',$post['srcUrl'])){
	//�Ō��srcfile�������邱�Ƃŏ�����؂蕪��
	if(!get_itemelements_union($post['tableName'],$post['startNum'],$itemelements,$message_codes,$dbhNIS,$_SESSION['dv_user_id'])){
		//�G���[���̏���
		echo $message_codes."\r\n";
		exit;
	}
}else{
	if(!get_itemelements($post['tableName'],$post['startNum'],$itemelements,$message_codes,$dbhNIS)){
		//�G���[���̏���
		echo $message_codes."\r\n";
		exit;
	}
}
//�L�[�i�e�[�u�����{���x���j�A�e�[�u�����A���x���A�J�n�ʒu�A�T�C�Y�A�f�[�^�^�C�v�A�\��
foreach($itemelements as $iteme){
	echo htmlEscape($iteme['tl']).           '//';
	echo htmlEscape($iteme['TableName']).    '//';
	echo htmlEscape($iteme['Label']).        '//';
	echo htmlEscape($iteme['S_Point']).      '//';
	echo htmlEscape($iteme['Size']).         '//';
	echo htmlEscape($iteme['DataType']).     '//';
	echo htmlEscape($iteme['Japanese_Name'])."\r\n";
}


?>
