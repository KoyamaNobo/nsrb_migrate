<?php
// process statusを取得、整形するクラス
// author : koyama
// create : 20170510
//このクラスはローカル変数をスネーク型で記述する
if(!defined(CHARSET)){
	define(CHARSET,'UTF-8');
}
class clsDBStat{
	private $proc_array;
	private $oLog;
	//ps lx を使ってみたが,実行開始時間が取れなかった
	const SELECT   = 'SELECT TableName,LockMode,pg_name,user_name,LockDateTime ';
	const FROM   = 'FROM M_TBLMANAGE ';
	const WHERE = ' WHERE Uid IS NOT NULL ';
	const ORDER_BY = ' ORDER BY LockDateTime DESC ';
	const LIST_LINE = 10;
	public $title = '';

	///
	function __construct(){
		require_once('./lib/log.php');
		$this->proc_array = array();
		$this->title      = mb_convert_encoding("実行中プロセス",CHARSET);
		$sql = self::SELECT.self::FROM. self::WHERE .self::ORDER_BY .';';
		$this->oLog = New Log('');
		//列名は空で作れる
		$this->proc_array[] = New clsDBStatusElem('');
		try{
	//		$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, '');
			$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, DB_PASS);
		}catch (Exception $e) {
			session_destroy();
			$mess = 'Can not Connect Database <br />'.$e->getMessage();
		}
		//データの取得
		$sth = $dbh->prepare($sql);
		$sth->execute();
		if (!$sth) {
		    $this->oLog->info("\nPDO::errorInfo():\n".print_r($dbh->errorInfo(),TRUE));
		}
		while($db_status = $sth->fetch(PDO::FETCH_ASSOC)){
			//psの結果を配列に変換してプライベート変数へ格納
			$cls = New clsDBStatusElem($db_status);
			//表示情報として必要ないものは表示しない
			// (DBの方は基本的に意味は無いが形を同じにしておく)
			if($this->unnecessaryElem($cls) == 1){
				continue;
			}
			$this->proc_array[] = $cls;
		}
	}

	///HTMLとして返す行をまとめて取得する
	public function getLineWithPageNum($page_num){
		$return_string  ='';
		$return_string .='<div class="statusBlock">';
		for($ii = 0;$ii < self::LIST_LINE;$ii++){
			$view_num = ($page_num * ($ii + 1)) - 1;
			$return_string .= $this->getLineWithLineNum($view_num);
		}
		//ページ番号を埋める
		$return_string .='<div class="process footer">'.PHP_EOL;
		$return_string .='<span>-'.$page_num.'-</span>'.PHP_EOL;
		$return_string .='</div>'.PHP_EOL;
		$return_string .='</div>';
		return $return_string;
	}

	///HTMLとして返す1行を作る
	function getLineWithLineNum( $num ){
		$return_string='';
//		$this->oLog->info('check00 num'.$num .' key_exists'.array_key_exists($num,$this->proc_array).__FILE__.':'.__LINE__);
		//keyが存在しないときは底までしか行がなかったとして空で反応
		if(!array_key_exists($num,$this->proc_array)){
			return $return_string;
		}
		if($this->proc_array[$num]->getHeader_Flg() == 1){
			$return_string .= '<div class="process header">'.PHP_EOL;
		}else{
			$return_string .= '<div class="process">'.PHP_EOL;
		}
		$return_string .= '<span class="stnum">';
		if($num != 0){
			$return_string .= sprintf("%03d",$num);
		}else{
			$return_string .= mb_convert_encoding("連番",CHARSET);
		}
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="sttbname">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getName(),0,30,'...',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stuser">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getUser(),0,8,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		// $return_string .= '<span class="stid">';
		// $return_string .= mb_strimwidth($this->proc_array[$num]->getId(),0,8,'-',CHARSET);
		// $return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stlocktime">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getLockTime(),0,20,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stpgname">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getJobName(),0,20,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="ststatus">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getStatus(),0,6,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
//		$return_string .= '<pre>';
//		$return_string .= $this->proc_array[$num]->getAll();
//		$return_string .= '</pre>'.PHP_EOL;
		$return_string .='</div>'.PHP_EOL;
		return $return_string;
	}
	//取得行数全てを表示
	public function countStatusLine(){
		//ヘッダ行があるので -1
		return count($this->proc_array) - 1;
	}
	//表示ページ数の最大
	public function countStatusMax(){
		//ヘッダ行があるので -1
		return ceil((count($this->proc_array) - 1) / self::LIST_LINE);
	}

	//$processStatusElemで画面表示に必要ないものかどうかを判断
	private function unnecessaryElem($processStatusElem){
		$unnecessaryFlg = 0;
		return $unnecessaryFlg;
	}
}

///プロセスの情報を記録する
class clsDBStatusElem{
	private $user;
	private $header_flg;
	private $name;
	private $status;
	private $lock_time;
	private $pid;
	private $job_name;

	///ps auxのときの最適化
	function __construct($st_array){
		$this->name        = mb_convert_encoding("名前",CHARSET);
		$this->user        = mb_convert_encoding("ユーザ",CHARSET);
		$this->status      = mb_convert_encoding("モード",CHARSET);
		$this->lock_time   = mb_convert_encoding("ロック時間",CHARSET);
		$this->job_name    = mb_convert_encoding("PG名",CHARSET);
		$this->header_flg  = 1;
		$this->all         = '';
		//空文字列ならヘッダ
		if(!empty($st_array)){
			$this->field_parse($st_array);
		}
		// $this->string_parse_alx($string_array);
	}
	//プロセスIDのGet
	public function getHeader_Flg(){
		return $this->header_flg;
	}
	//プロセスIDのGet
	public function getPid(){
		return $this->$pid;
	}
	//テーブル名のGet
	public function getName(){
		return $this->name;
	}
	//ユーザーのGet
	public function getUser(){
		return $this->user;
	}
	//PG名のGet
	public function getJobName(){
		return $this->job_name;
	}

	//ロック時間のGet
	public function getLockTime(){
		return $this->lock_time;
	}

	//状態のGet
	public function getStatus(){
		//とりあえずそのまま入れる
		$ret_val = $this->status;
		// if(preg_match('/(S|O)/',$this->status)){
		// 	$ret_val = mb_convert_encoding("実行",CHARSET);
		// }
		return $ret_val;
	}

	//全てのGet
	public function getAll(){
		return $this->all;
	}

	private function takeParentId($childId){
		if(is_numeric($childId)){
			$ret_val = shell_exec("ps alx|awk '$3==\"".$childId."\" { print $4 }'");
			$this->parent_id = trim($ret_val);
		}
	}

	//ps auxで取ったときのstring_parse
	function field_parse($string_array){
		//string_arrayが空のときは一度も通らない
		$this->header_flg  = 0;
		$this->name        = $string_array['TableName'];
		$this->status      = $string_array['LockMode'];
		$this->job_name    = $string_array['pg_name'];
		$this->user        = $string_array['user_name'];
		$this->lock_time   = $string_array['LockDateTime'];
	}
}
?>
