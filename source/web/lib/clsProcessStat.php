<?php
// process statusを取得、整形するクラス
// author : koyama
// create : 20170510
//このクラスはローカル変数をスネーク型で記述する
if(!defined('CHARSET')){
	define('CHARSET','UTF-8');
}
class clsProcessStat{
	private $proc_array;
	private $oLog;
	//ps lx を使ってみたが,実行開始時間が取れなかった
	// const CMD = 'ps aux | grep -v "sbin" |grep -v grep |grep -v "ps"|grep apache ';
	const CMD = 'ps lx --no-header | grep -v "bash" | grep -v "sbin"|grep -v grep |grep -v "ps"';
	const LIST_LINE = 10;
	public $title = '';

	///
	function __construct(){
		require_once('./lib/log.php');
		$this->oLog = New Log('');
		$this->proc_array = array();
		$this->title      = mb_convert_encoding("実行中プロセス",CHARSET);
		$mixed_array = explode(PHP_EOL,shell_exec(self::CMD));
		$now_time = time();
		//列名は空で作れる
		$this->proc_array[] = New clsProcessStatusElem('');
		foreach($mixed_array as $ret_line){
			//psの結果を配列に変換してプライベート変数へ格納
			if(strlen(trim($ret_line)) == 0){
				continue;
			}
			// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
			$split_string = preg_split('/\s+/',$ret_line,NULL,PREG_SPLIT_NO_EMPTY);
			$cls = New clsProcessStatusElem($split_string,$now_time);
			//表示情報として必要ないものは表示しない
			if($this->unnecessaryElem($cls) == 1){
				continue;
			}
			$this->proc_array[] = $cls;
		}
	}

	///HTMLとして返す行をまとめて取得する
	public function getLineWithPageNum($page_num){
		$return_string  ='';
		$return_string .='<div class="statusBlock">'.PHP_EOL;
		//ヘッダの出力
		$return_string .= $this->getLineWithLineNum(0);
		for($ii=1;$ii <= self::LIST_LINE;$ii++){
			if($page_num > 1){
				$view_num = (self::LIST_LINE * ($page_num - 1))+ $ii;
			}else{
				$view_num = $ii;
			}
			$return_string .= $this->getLineWithLineNum($view_num);
		}
		//ページ番号を埋める
		$return_string .='<div class="process footer">'.PHP_EOL;
		$return_string .='<span>-'.$page_num.'-</span>'.PHP_EOL;
		$return_string .='</div>'.PHP_EOL;
		$return_string .='</div>'.PHP_EOL;
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
		$return_string .= '<span class="stid">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getId(),0,8,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stid">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getParentId(),0,8,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stuser">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getUser(),0,8,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="ststart">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getStart(),0,11,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stlapsed">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getLapsed(),0,8,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="ststatus">';
		$return_string .= mb_strimwidth($this->proc_array[$num]->getStatus(),0,6,'-',CHARSET);
		$return_string .= '</span>'.PHP_EOL;
		$return_string .= '<span class="stname">';
		$nbsp_pos = strpos($this->proc_array[$num]->getName(),'&nbsp');
		if($nbsp_pos > 10 && $nbsp_pos < 18){
			//スペースが途中にあり、文字数が超えない場合底までを出力
			$return_string .= substr($this->proc_array[$num]->getName(),0,$nbsp_pos);
		}else{
			$return_string .= mb_strimwidth($this->proc_array[$num]->getName(),0,18,'...',CHARSET);
		}
		$return_string .= '</span>'.PHP_EOL;
		// $return_string .= '<pre>';
		// $return_string .= $this->proc_array[$num]->getAll();
		// $return_string .= '</pre>'.PHP_EOL;

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
		//ajaxtermが動いている可能性
		if(preg_match('/^ps[|&].*/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		//teeが含まれるものは表示に必要ない 後ろは&nbsp;に変換しているはず
		if(preg_match('/^(.*[|;])?tee[|&].*/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		//ajaxtermが動いている可能性
		if(preg_match('/^python[|&].*/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		//awkが入っているものはプロセス取得系
		if(preg_match('/awk.*/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		//標準入出力があるものは要らない？
		if(preg_match('/[><|]/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		//runExecは実行管理しているだけ
		if(preg_match('/.*runExec.php.*/',$processStatusElem->getName()) == 1){
			$unnecessaryFlg = 1;
		}
		return $unnecessaryFlg;
	}
}

///プロセスの情報を記録する
class clsProcessStatusElem{
	private $name;
	private $all;
	private $header_flg;
	private $start_time;
	private $lapsed_time;
	private $id;
	private $user;
	private $status;
	private $parent_id;
	private $oLog;

	///ps auxのときの最適化
	function __construct($string_array,$now){
		require_once('./lib/log.php');
		$this->oLog = New Log('');
		$this->header_flg  = 1;
		$this->id          = mb_convert_encoding("ID",CHARSET);
		$this->parent_id   = mb_convert_encoding("親ID",CHARSET);
		$this->name        = mb_convert_encoding("名前",CHARSET);
		$this->start_time  = mb_convert_encoding("開始時間",CHARSET);
		$this->lapsed_time = mb_convert_encoding("経過時間",CHARSET);
		$this->user        = mb_convert_encoding("ユーザ",CHARSET);
		$this->status      = mb_convert_encoding("状態",CHARSET);
		$this->all         = '';
		//空文字列ならヘッダ
		if(!empty($string_array)){
			// $this->string_parse_aux($string_array);
			// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
			$this->string_parse_alx($string_array,$now);
			//ここはalxならいらない。
			// $this->takeParentId($this->id);

		}
		// $this->string_parse_alx($string_array);
	}
	//プロセスIDのGet
	public function getHeader_Flg(){
		return $this->header_flg;
	}
	//プロセスIDのGet
	public function getId(){
		return $this->id;
	}
	//親プロセスIDのGet
	public function getParentId(){
		return $this->parent_id;
	}

	//プロセス名のGet
	public function getName(){
		return $this->name;
	}
	//開始時間のGet
	public function getStart(){
		return $this->start_time;
	}

	//経過時間のGet
	public function getLapsed(){
		return $this->lapsed_time;
	}
	//ユーザーのGet
	public function getUser(){
		return $this->user;
	}
	//状態のGet
	public function getStatus(){
		//とりあえずそのまま入れる
		$ret_val = $this->status;
		if(preg_match('/(S|O|R)/',$this->status)){
			$ret_val = mb_convert_encoding("実行",CHARSET);
		}
		if(preg_match('/T/',$this->status)){
			$ret_val = mb_convert_encoding("停止",CHARSET);
		}
		return $ret_val;
	}

	//全てのGet
	public function getAll(){
		return $this->all;
	}

	private function takeParentId($childId){
		if(is_numeric($childId)){
			$ret_val = shell_exec("ps l --no-header --pid=\"".$childId."\" |awk '{ print $4 }'");
			$this->parent_id = trim($ret_val);
		}
	}

	//ps auxで取ったときのstring_parse
	function string_parse_aux($string_array){
		//string_arrayが空のときは一度も通らない
		for($ii=0;$ii < count($string_array);$ii++){
			//区切り記号を*にしておく
			$this->all       .= '*'.$string_array[$ii];
			switch ($ii) {
				case 0:
				case 2:
				case 3:
				case 4:
				case 5:
				case 6:
					break;
				case 1:
					//一つ以上あるときは空初期化
					$this->name       = '';
					//idがあるときはheaderではない
					$this->header_flg = 0;
					$this->id = $string_array[$ii];
					break;
				case 7:
					//状態
					$this->status = $string_array[$ii];
					break;
				case 8:
					//開始時間
					$this->start_time = $string_array[$ii];
					break;
				case 9:
					//経過時間
					$this->lapsed_time = $string_array[$ii];
					break;
				case 10:
					//プロセス名の先頭
					$this->name  = $string_array[$ii].'&nbsp;';
					break;
				case 11:
					//ユーザのはず
					$this->user  = $string_array[$ii];
					$this->name  .= $string_array[$ii].'&nbsp;';
					break;
				default:
					//それ以降 プロセス名
					//sh -cの記述は起動の指定
					if(preg_match('/^sh.*-c&nbsp;$/',$this->name)){
						$this->name  = $string_array[$ii].'&nbsp;';
						$this->user  = $string_array[$ii + 1];
					}else if(preg_match('/tcsh&nbsp;/',$this->name)){
						//何故かスペース一つが切れていない
						$this->name  = preg_replace('/.*tcsh&nbsp;/','',$this->name) . $string_array[$ii].'&nbsp;';
						$this->user  = $string_array[$ii];
					} else {
						$this->name  .= $string_array[$ii].'&nbsp;';
					}
					break;
			}
		}
	}
	//ps alxで取ったときのstring_parse
	function string_parse_alx($string_array,$now){
		//string_arrayが空のときは一度も通らない
		for($ii=0;$ii < count($string_array);$ii++){
			//区切り記号を*にしておく
			$this->all       .= '*'.$string_array[$ii];
			switch ($ii) {
				case 0:
				case 1:
					break;
				case 2:
					//一つ以上あるときは空初期化
					$this->name = '';
					//idがあるときはheaderではない
					$this->header_flg = 0;
					$this->id = $string_array[$ii];
					break;
				case 3:
					$this->parent_id = $string_array[$ii];
					break;
				case 4:
				case 5:
				case 6:
				case 7:
				case 8:
				case 10:
					break;
				case 9:
					//状態
					$this->status = $string_array[$ii];
					break;
				case 11:
					//経過時間
					$this->lapsed_time = $string_array[$ii];
					//開始時間(誤差は許容することにする)
					if(preg_match('/^[0:]+$/',$this->lapsed_time) == 0){
						$this->start_time =
						date('m/d H:i',
							strtotime(
								preg_replace('/([0-9]+):([0-9]+)/','-${1} minute -${2} second '
								,$this->lapsed_time)
							)
						);
					}else{
						$this->start_time = '';
					}
					break;
				case 12:
					//プロセス名の先頭
					$this->name  = $string_array[$ii].'&nbsp;';
					break;
				case 13:
					//ユーザのはず
					$this->user  = $string_array[$ii];
					$this->name  .= $string_array[$ii].'&nbsp;';
					break;
				default:
					//それ以降 プロセス名
					//sh -cの記述は起動の指定
					if(preg_match('/^sh.*-c&nbsp;$/',$this->name)){
						$this->name  = $string_array[$ii].'&nbsp;';
						$this->user  = $string_array[$ii + 1];
					}else if(preg_match('/tcsh&nbsp;/',$this->name)){
						//何故かスペース一つが切れていない
						$this->name  = preg_replace('/.*tcsh&nbsp;/','',$this->name) . $string_array[$ii].'&nbsp;';
						$this->user  = $string_array[$ii];
					} else {
						$this->name  .= $string_array[$ii].'&nbsp;';
					}
					break;
			}
		}
	}
}
?>
