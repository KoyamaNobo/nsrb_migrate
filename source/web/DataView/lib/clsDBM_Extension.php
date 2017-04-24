<?php
//DB����M_Extention�̃f�[�^���擾��cls�Ƃ��ĕۑ�
//Date: 20160524
//author:koyama
class clsDBM_Extension {
	public $pdoH;
	public $DBNAME;
	public $MAXNUM;
	public $user_id;
	public $tableData;
	public $err_code;    //���O�̊֐��̃G���[�R�[�h��ێ�
	public $whereState;    //post���Ŏ󂯎�����p�����[�^

	//�R���X�g���N�^�ŕK�v�ȃf�t�H���g�ݒ��������ăZ�b�g
	//author:koyama
	function __construct(&$dbH,$user_id,$DBNAME,$MAXNUM){
		$this->pdoH      = $dbH;
		$this->err_code  ='';            //�G���[�R�[�h�͋󕶎���ŏ�����
		$this->user_id   =$user_id;      //���O�C�����[�U��ID
		$this->DBNAME    =$DBNAME;       //�ς��\��������̂ŁA�����Ɏ󂯂Ă���
		$this->MAXNUM    =$MAXNUM;       //�ς��\��������̂ŁA�����Ɏ󂯂Ă���
// 		shell_exec('logger -i "^MAXNUM^'.$MAXNUM.'"');
		$this->tableData = array();
		$this->whereState = New clsRowM_Extention();
	}

	//extension id�𐶐�
	//author:koyama
	function createExtensionId(){
		if(empty($this->whereState->extension_id)){
			//�f�[�^�x�[�X�̌��݂�ID�̒�����10,���ΏՓː����l���Ă�10����Ώ[��36^10�p�^�[���ɂȂ�͂�
			$this->whereState->extension_id = substr(hash('sha256',microtime()),0,10);
		}
	}

	//post�f�[�^���󂯎��
	//author:koyama
	function setState($postParam){
		$this->whereState->extension_id       = $postParam['Extension_Id'];
		$this->whereState->user_id            = $postParam['User_Id'];
		$this->whereState->tablename          = $postParam['TableName'];
		$this->whereState->item_japanese_name = $postParam['JapaneseName'];
		$this->whereState->s_point            = $postParam['S_point'];
		$this->whereState->size               = $postParam['Size'];
		$this->whereState->data_type          = $postParam['Data_Type'];
		$this->whereState->priority_disp_flg  = $postParam['NonDisp_Flg'];
		$this->whereState->del_flg            = '0';
	}

	//���݂�State�ōs�����擾
	//����:type �ǂ̂悤�ȏ����ł̍s�����擾���邩
	//return :�s��
	//date:20160525
	//author:koyama
	function getRowCountWithState($type = 'id'){
		$rowCount = 0;
		if($type === 'id'){
			//Extension_id���󂾂����瑶�݂��Ȃ��������Ƃɂ���
			if(!empty($this->whereState->extension_id)){
				$sql  = " SELECT count(Extension_Id) as num FROM ".$this->DBNAME.".m_extension ";
				$sql .= " WHERE Extension_Id=:Extension_Id ";
				$sth = $this->pdoH->prepare($sql);
				$sth->bindParam('Extension_Id',$this->whereState->extension_id);
				$sth->execute();
				//�G���[��������Ŏ��Ȃ��������̂��Ƃ��l���Ĉꉞwhile
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0���������Ă���̂�else�Ȃ�
		}else if($type === 'update'){
			//Extension_id���󂾂����瑶�݂��Ȃ��������Ƃɂ���
			if(!empty($this->whereState->extension_id)){
				$sql  = '';
				$sql .= ' SELECT count(Extension_Id) as num FROM '.$this->DBNAME.'.m_extension ';
				$sql .= ' WHERE Extension_Id=:Extension_Id ';
				$sql .= ' AND User_Id = :User_Id AND TableName = :TableName ';
				$sql .= ' AND Item_Japanese_Name = :Item_Japanese_Name ';
				$sql .= ' AND S_Point = :S_Point AND Size = :Size ';
				$sql .= ' AND Data_Type = :Data_Type AND Priority_Disp_Flg = :Priority_Disp_Flg ';
				$sql .= ' ; ';
				$sth = $this->pdoH->prepare($sql);
				$sth->bindParam('Extension_Id'      ,$this->whereState->extension_id);
				$sth->bindParam('User_Id'           ,$this->whereState->user_id);
				$sth->bindParam('TableName'         ,$this->whereState->tablename);
				$sth->bindParam('Item_Japanese_Name',$this->whereState->item_japanese_name);
				$sth->bindParam('S_Point'           ,$this->whereState->s_point);
				$sth->bindParam('Size'              ,$this->whereState->size);
				$sth->bindParam('Data_Type'         ,$this->whereState->data_type);
				$sth->bindParam('Priority_Disp_Flg' ,$this->whereState->priority_disp_flg);
				$sth->execute();
				//�G���[��������Ŏ��Ȃ��������̂��Ƃ��l���Ĉꉞwhile
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0���������Ă���̂�else�Ȃ�
		}else if($type === 'del'){
			//Extension_id���󂾂����瑶�݂��Ȃ��������Ƃɂ���
			if(!empty($this->whereState->extension_id)){
				$sql  = " SELECT count(Extension_Id) as num FROM ".$this->DBNAME.".m_extension ";
				$sql .= " WHERE Extension_Id=:Extension_Id AND Del_Flg = 1";
				$sth = $this->pdoH->prepare($sql);
				$sth->bindParam('Extension_Id',$this->whereState->extension_id);
				$sth->execute();
				//�G���[��������Ŏ��Ȃ��������̂��Ƃ��l���Ĉꉞwhile
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0���������Ă���̂�else�Ȃ�
		}

		//int�ɕϊ����ĕԂ�(�ϊ��s�\�ȕ����񂾂�0�ɂȂ�)
		return (int)$rowCount;
	}

	//���݂�State��insert(id�͂��̒��Ő�������
	//����:�Ȃ�
	//return :�s��
	//date:20160525
	//author:koyama
	function insertData(){
		//���݂��Ă��Ȃ��Ƃ��̓C���T�[�g
		$sql  = '';
		$sql  .= 'INSERT INTO '.$this->DBNAME.'.m_extension (';
		$sql  .= '   Extension_Id, User_Id, TableName ';
		$sql  .= ' , Item_Japanese_Name, S_Point, Size ';
		$sql  .= ' , Data_Type, Priority_Disp_Flg ';
		$sql  .= ' , Cre_Date, Mod_Date, Del_Flg ';
		$sql  .= ') VALUES ( ';
		$sql  .= '   :Extension_Id, :User_Id, :TableName ';
		$sql  .= ' , :Item_Japanese_Name, :S_Point, :Size ';
		$sql  .= ' , :Data_Type, :Priority_Disp_Flg ';
		$sql  .= ' , now(), now() , :Del_Flg ';
		$sql  .= '); ';
		$sth = $this->pdoH->prepare($sql);

		$this->createExtensionId();
		$sth->bindParam('Extension_Id'      ,$this->whereState->extension_id);
		$sth->bindParam('User_Id'           ,$this->whereState->user_id);
		$sth->bindParam('TableName'         ,$this->whereState->tablename);
		$sth->bindParam('Item_Japanese_Name',$this->whereState->item_japanese_name);
		$sth->bindParam('S_Point'           ,$this->whereState->s_point);
		$sth->bindParam('Size'              ,$this->whereState->size);
		$sth->bindParam('Data_Type'         ,$this->whereState->data_type);
		$sth->bindParam('Priority_Disp_Flg' ,$this->whereState->priority_disp_flg);
		$sth->bindParam('Del_Flg'           ,$this->whereState->del_flg);

		$sth->execute();

		//��ō����ID�̃f�[�^���쐬����Ă��邩�`�F�b�N�����̐���return
		$count = $this->getRowCountWithState();
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;
	}
	//���݂�State��insert(id�͂��̒��Ő�������
	//����:�Ȃ�
	//return :�s��
	//date:20160525
	//author:koyama
	function updateData(){
		//���݂��Ă���Ƃ���update
		$sql  = '';
		$sql  .= '  UPDATE '.$this->DBNAME.'.m_extension SET ';
		$sql  .= '  User_Id = :User_Id, TableName = :TableName ';
		$sql  .= ' , Item_Japanese_Name = :Item_Japanese_Name ';
		$sql  .= ' , S_Point = :S_Point, Size = :Size ';
		$sql  .= ' , Data_Type = :Data_Type, Priority_Disp_Flg = :Priority_Disp_Flg ';
		$sql  .= ' , Mod_Date=now() ';
		$sql  .= ' WHERE ';
		$sql  .= ' Extension_Id = :Extension_Id ';
		$sql  .= ' ; ';
		$sth = $this->pdoH->prepare($sql);

		$sth->bindParam('Extension_Id'      ,$this->whereState->extension_id);
		$sth->bindParam('User_Id'           ,$this->whereState->user_id);
		$sth->bindParam('TableName'         ,$this->whereState->tablename);
		$sth->bindParam('Item_Japanese_Name',$this->whereState->item_japanese_name);
		$sth->bindParam('S_Point'           ,$this->whereState->s_point);
		$sth->bindParam('Size'              ,$this->whereState->size);
		$sth->bindParam('Data_Type'         ,$this->whereState->data_type);
		$sth->bindParam('Priority_Disp_Flg' ,$this->whereState->priority_disp_flg);

		$sth->execute();

		//��ō����ID�̃f�[�^���쐬����Ă��邩�`�F�b�N�����̐���return
		$count = $this->getRowCountWithState('update');
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;
	}

	//���݂�State��insert(id�͂��̒��Ő�������
	//����:�Ȃ�
	//return :�s��
	//date:20160525
	//author:koyama
	function delData(){
		$sql   = '';
// 		$sql  .= 'DELETE FROM '.$this->DBNAME.'.m_extension ';
// 		$sql  .= ' WHERE ';
// 		$sql  .= ' Extension_Id = :Extension_Id ';
// 		$sql  .= ' ; ';
		$sql  .= 'UPDATE '.$this->DBNAME.'.m_extension SET ';
		$sql  .= ' Del_Flg = 1 ';
		$sql  .= ' ,Mod_Date = now() ';
		$sql  .= ' WHERE ';
		$sql  .= ' Extension_Id = :Extension_Id ';
		$sql  .= ' ; ';
		$sth = $this->pdoH->prepare($sql);
		$sth->bindParam('Extension_Id'      ,$this->whereState->extension_id);
		//SQL���s
		$sth->execute();

		//��ō����ID�̃f�[�^���쐬����Ă��邩�`�F�b�N�����̐���return
		$count = $this->getRowCountWithState('del');
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;

	}

	//����̍i�荞�ݏ��������ł��̃��[�U���{���\�ȃf�[�^���擾�v���p�e�B�֊i�[
	//return :�G���[�R�[�h(���ݎd�l�ł̓G���[�̉\���Ȃ��Ɣ��f
	//author:koyama
	function get_items($num){
		$errCode         = '';    //���۔���
		$this->err_code  ='';    //�O�̏��ƍ������Ȃ����߂ɏ�����
		$this->tableData = array();
		$roopCount     = 0;
		$sql           = '';
		$sql .= 'SELECT ';
		$sql .= ' Extension_Id, User_Id, TableName, Item_Japanese_Name ';
		$sql .= ' , S_Point, Size, Data_Type, Priority_Disp_Flg ';
		$sql .= ' , Del_Flg ';
		$sql .= 'FROM '.$this->DBNAME.'.m_extension ';
		$sql .= 'WHERE NOT(Priority_Disp_Flg = 0 AND User_Id <> :user_id ) ';
		$sql .= 'AND Del_Flg = 0 ';
		$sql .= 'ORDER BY TableName ,Item_Japanese_Name ,S_Point ,Size ';
		$sql .= 'LIMIT '. (int)($this->MAXNUM * $num) .' , '. (int)($this->MAXNUM) . ' ; ';
		$sth = $this->pdoH->prepare($sql);
		$sth->bindParam('user_id',$this->user_id);
		//SQL���s
		shell_exec('logger -i "^SQL^'.$sql.'"');
		shell_exec('logger -i "^num^'.($this->MAXNUM * $num).'"');
		$sth->execute();
		while($row = $sth->fetch(PDO::FETCH_ASSOC)){
			$temp = New clsRowM_Extention();
			$temp->extension_id       = $row['Extension_Id'];
			$temp->user_id            = $row['User_Id'];
			$temp->tablename          = $row['TableName'];
			$temp->item_japanese_name = $row['Item_Japanese_Name'];
			$temp->s_point            = $row['S_Point'];
			$temp->size               = $row['Size'];
			$temp->data_type          = $row['Data_Type'];
			$temp->priority_disp_flg  = $row['Priority_Disp_Flg'];
			$temp->del_flg            = $row['Del_Flg'];
			array_push($this->tableData,$temp);
		}
		return $errCode;
	}
}

//M_Extention�e�[�u���̈�s���\����
//Date: 20160524
//author:koyama
class clsRowM_Extention{
	public $extension_id;
	public $user_id;
	public $tablename;
	public $item_japanese_name;
	public $s_point;
	public $size;
	public $data_type;
	public $priority_disp_flg;
	public $cre_date;
	public $mod_date;
	public $del_flg;

	function __construct(){
		$this->extension_id       = '';
		$this->user_id            = '';
		$this->tablename          = '';
		$this->item_japanese_name = '';
		$this->s_point            = '';
		$this->size               = '';
		$this->data_type          = '';
		$this->priority_disp_flg  = '';
		$this->del_flg            = '';
	}

	function getExtension_id(){
		if(strlen($this->extension_id) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->extension_id);
		}
	}
	function getTablename(){
		if(strlen($this->tablename) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->tablename);
		}
	}
	function getUser_id(){
		if(strlen($this->user_id) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->user_id);
		}
	}
	function getItem_japanese_name(){
		if((strlen(bin2hex($this->item_japanese_name)) / 2) > 20){
			return substr(htmlEscape($this->item_japanese_name),0,18) . '...';
		}else if(strlen($this->item_japanese_name) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->item_japanese_name);
		}
	}
	function getDisp_flg(){
		if($this->priority_disp_flg == 0){
			return '�����̂�';
		}else{
			return '���l��';
		}
	}
	function getS_point(){
		if(strlen($this->s_point) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->s_point);
		}
	}
	function getSize(){
		if(strlen($this->size) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->size);
		}
	}
	function getData_type(){
		if(strlen($this->data_type) == 0){
			return htmlEscape(' ');
		}else{
			return htmlEscape($this->data_type);
		}
	}
}
?>
