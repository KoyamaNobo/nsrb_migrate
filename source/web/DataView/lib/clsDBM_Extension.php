<?php
//DBからM_Extentionのデータを取得しclsとして保存
//Date: 20160524
//author:koyama
class clsDBM_Extension {
	public $pdoH;
	public $DBNAME;
	public $MAXNUM;
	public $user_id;
	public $tableData;
	public $err_code;    //直前の関数のエラーコードを保持
	public $whereState;    //post等で受け取ったパラメータ

	//コンストラクタで必要なデフォルト設定をもらってセット
	//author:koyama
	function __construct(&$dbH,$user_id,$DBNAME,$MAXNUM){
		$this->pdoH      = $dbH;
		$this->err_code  ='';            //エラーコードは空文字列で初期化
		$this->user_id   =$user_id;      //ログインユーザのID
		$this->DBNAME    =$DBNAME;       //変わる可能性があるので、ここに受けておく
		$this->MAXNUM    =$MAXNUM;       //変わる可能性があるので、ここに受けておく
// 		shell_exec('logger -i "^MAXNUM^'.$MAXNUM.'"');
		$this->tableData = array();
		$this->whereState = New clsRowM_Extention();
	}

	//extension idを生成
	//author:koyama
	function createExtensionId(){
		if(empty($this->whereState->extension_id)){
			//データベースの現在のIDの長さが10,かつ対衝突性を考えても10あれば充分36^10パターンになるはず
			$this->whereState->extension_id = substr(hash('sha256',microtime()),0,10);
		}
	}

	//postデータを受け取り
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

	//現在のStateで行数を取得
	//引数:type どのような条件での行数を取得するか
	//return :行数
	//date:20160525
	//author:koyama
	function getRowCountWithState($type = 'id'){
		$rowCount = 0;
		if($type === 'id'){
			//Extension_idが空だったら存在しなかったことにする
			if(!empty($this->whereState->extension_id)){
				$sql  = " SELECT count(Extension_Id) as num FROM ".$this->DBNAME.".m_extension ";
				$sql .= " WHERE Extension_Id=:Extension_Id ";
				$sth = $this->pdoH->prepare($sql);
				$sth->bindParam('Extension_Id',$this->whereState->extension_id);
				$sth->execute();
				//エラーだったりで取れなかった時のことを考えて一応while
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0初期化しているのでelseなし
		}else if($type === 'update'){
			//Extension_idが空だったら存在しなかったことにする
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
				//エラーだったりで取れなかった時のことを考えて一応while
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0初期化しているのでelseなし
		}else if($type === 'del'){
			//Extension_idが空だったら存在しなかったことにする
			if(!empty($this->whereState->extension_id)){
				$sql  = " SELECT count(Extension_Id) as num FROM ".$this->DBNAME.".m_extension ";
				$sql .= " WHERE Extension_Id=:Extension_Id AND Del_Flg = 1";
				$sth = $this->pdoH->prepare($sql);
				$sth->bindParam('Extension_Id',$this->whereState->extension_id);
				$sth->execute();
				//エラーだったりで取れなかった時のことを考えて一応while
				while($item = $sth->fetch(PDO::FETCH_ASSOC)){
					$rowCount = $item['num'];
				}
			}
			//0初期化しているのでelseなし
		}

		//intに変換して返す(変換不可能な文字列だと0になる)
		return (int)$rowCount;
	}

	//現在のStateでinsert(idはこの中で生成する
	//引数:なし
	//return :行数
	//date:20160525
	//author:koyama
	function insertData(){
		//存在していないときはインサート
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

		//上で作ったIDのデータが作成されているかチェックしその数をreturn
		$count = $this->getRowCountWithState();
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;
	}
	//現在のStateでinsert(idはこの中で生成する
	//引数:なし
	//return :行数
	//date:20160525
	//author:koyama
	function updateData(){
		//存在しているときはupdate
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

		//上で作ったIDのデータが作成されているかチェックしその数をreturn
		$count = $this->getRowCountWithState('update');
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;
	}

	//現在のStateでinsert(idはこの中で生成する
	//引数:なし
	//return :行数
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
		//SQL実行
		$sth->execute();

		//上で作ったIDのデータが作成されているかチェックしその数をreturn
		$count = $this->getRowCountWithState('del');
		$ret = false;
		if($count > 0){
			$ret = true;
		}
		return $ret;

	}

	//特定の絞り込み条件無しでそのユーザが閲覧可能なデータを取得プロパティへ格納
	//return :エラーコード(現在仕様ではエラーの可能性なしと判断
	//author:koyama
	function get_items($num){
		$errCode         = '';    //成否判定
		$this->err_code  ='';    //前の情報と混同しないために初期化
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
		//SQL実行
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

//M_Extentionテーブルの一行を構造化
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
			return '自分のみ';
		}else{
			return '他人も';
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
