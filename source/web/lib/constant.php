<?php
///
///変わることのない定数を分割
///
define('HTML_CLASS_REVERSE','reverse');       //classとしてのreverseの指定
define('PHP_CLASS_REVERSE','REV');            //PHPでの区分としてのreverseの指定
define('HTML_CLASS_BLINK','blink');           //classとしてのblinkの指定
define('PHP_CLASS_BLINK','BLI');            //PHPでの区分としてのreverseの指定

require_once('./lib/log.php');
class initConf{
	private $arrTopMenu;
	private $arrKind;
	private $arrColorMap;  //表示用の文字列の定義
	private $intExtState;  //機能拡張の状態 0:通常 1:FileExplorer拡張
	private $arrExplorerPath;  //FileExplorer拡張機能の対象パス配列
	private $strExplorerPath;  //FileExplorer拡張機能の対象パス文字列

	function __construct(){
		$this->arrTopMenu  = array('imnu00','kmnu00','gmenu','hak00','koh00','kob00','mitu01','mitu03'
		                    ,'zai000','teg000','tama01','ryob01','ryob02');
		$this->arrKind     = array('PM','JS','LM','SM');
		$this->arrColorMap = array(
		                            '#131313'  => '黒',
		                            '#FFFFFF'  => '白',
		                            '#808080'  => '灰',
		                            '#00FFA5'  => '緑',
		                            '#DC143E'  => '赤',
		                            '#0000CD'  => '青',
		                            '#FFA500'  => '橙',
		                            '#00FFFF'  => '水',
		                            '#EE82EE'  => '紫',
		                           );
		$this->arrExplorerPath = array('/var/local/DataExchange/','../tmp/');
		$this->strExplorerPath = $this->arrExplorerPath[0];
		$this->intExtState = 0;  //intExtState [0:初期状態 1:FileExplorer]
	}

	//添え字をとして数字を入れてその番号に対応する機能名を返す
	//IN :添え字の数字
	//OUT:機能名(JS,LMなど)
	//author:koyama
	public function getKind($num){
		//添え字が存在しなければ0
		if(array_key_exists($num,$this->arrKind)){
			return $this->arrKind[$num];
		}else{
			return 0;
		}
	}

	//Explorer用のPathを返す
	//IN :なし
	//OUT:ファイルパスの文字列
	//author:koyama
	public function setstrExplorerPath($index){
		$this->intExtState = $index;
		$this->strExplorerPath = $this->arrExplorerPath[$index];
	}

	//Explorer用のPathを返す
	//IN :なし
	//OUT:ファイルパスの文字列
	//author:koyama
	public function getstrExplorerPath(){
		return $this->strExplorerPath;
	}

	//Explorer用の現在セットされている機能の番号を返す
	//IN :なし
	//OUT:機能の番号
	//author:koyama
	public function getintExtState(){
		return $this->intExtState;
	}

	//Explorer用で現在の機能名を元に次のページ遷移先を返す
	//IN :なし
	//OUT:ファイル名の文字列
	//author:koyama
	public function getFileExpDetail($location){
		$retVal = '';
		switch ($location){
		case 'index':
			$retVal = 'FileExplorerDetail.php';
			break;
		case 'detail':
			$retVal = 'FileExplorer.php';
			break;
		default:
			$retVal = 'FileExplorer.php';
		}
		return $retVal;
	}

	//添え字に当たる色の16進数または英名を入れて日本語名を返す
	//IN :色の16進数または英名(#FFFFFF,whiteなど)
	//OUT:日本語名(白など)
	//author:koyama
	public function getJapanColorName($cn){
		//添え字が存在しなければ0
		if(array_key_exists($cn,$this->arrColorMap)){
			return $this->arrColorMap[$cn];
		}else{
			return 'なし';
		}
	}

	public function getColorNameList(){
		return $this->arrColorMap;
	}

	public function setDefaultValue($val,$name){
		if(!is_null($val) && !empty($val)){
			return $val;
		}else{
			//デフォルトの値を返す
			$keys=array();
			switch($name){
				case 'reverse_font_color':
					$keys = array_keys($this->arrColorMap);
					return $keys[1];
				case 'reverse_bg_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[5];
				case 'bg_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[0];
				case 'font_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[3];
				case 'font_size':
					return MIN_F_SIZE + ((MAX_F_SIZE - MIN_F_SIZE) / 2);
			}
			return $keyname;
		}
	}

	//ログインできるかどうかのチェック
	//in:$acc->ユーザid(ログインアカウント名),$pass->ログインパスワード,
	//   $table->ユーザ情報テーブルのresult,$message->リターンするメッセージ格納
	public function loginCheck($acc, $pass ,&$message){
		$result     = false;
		$user_id    = '';
		$ser_name   = '';
		$password   = '';
		$def_name   = '';
		$font_size  = '';
		$font_color = '';
		$bg_color   = '';
		$pg_names   = array();
		$print_id   = '';
		$i = 0;        //foreachの際のループ変数

		try{
	//		$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, '');
	 		$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, DB_PASS);
		}catch (Exception $e) {
			session_destroy();
			$mess = 'Can not Connect Database <br />'.$e->getMessage();
			$mess = urlencode($mess);
			header('Location: ./sessDestroy.php?mess='.$mess);
		}
		//これでpermissionベースでログイン可能リストが取れる
		//SELECT * FROM M_USER MU join M_PG MP on MU.permission NOT like REPLACE(REPLACE(rpad(MP.permission,14,'0'),'0','_'),'1','0') order by user_id;
		$sql  = ' SELECT user_id,user_name,password,DEF_PG.pg_name as def_name,MP.permission as permission,print_id, MP.pg_name as pg_name ';
		$sql .= ' ,font_size,font_color,bg_color,reverse_font_color,reverse_bg_color ';
		$sql .= ' FROM M_USER MU ';
		$sql .= ' INNER JOIN M_PG DEF_PG ';
		$sql .= ' ON MU.pg_id = DEF_PG.pg_id ';
		$sql .= ' INNER JOIN M_PG MP ';
		$sql .= ' ON MU.permission NOT like REPLACE(REPLACE(rpad(MP.permission,CHAR_LENGTH(MP.permission),"0"),"0","_"),"1","0") ';
		$sql .= ' WHERE user_name=:user_name AND password=:password';
		$sth = $dbh->prepare($sql);
		$sth->bindParam('user_name',$acc);
		$sth->bindParam('password',$pass);
		$sth->execute();
	// 	$userTable = $sth->fetchAll();
	// 	foreach($userTable as $user){
	// echo $sql;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$user_id   = $user['user_id'];
			$user_name = $user['user_name'];
			$password  = $user['password'];
			$def_name  = $user['def_name'];
			array_push($pg_names, $user['pg_name']);
			$print_id  = $user['print_id'];
			$font_size = $this->setDefaultValue($user['font_size'],'font_size');
			$font_color= $this->setDefaultValue($user['font_color'],'font_color');
			$bg_color  = $this->setDefaultValue($user['bg_color'],'bg_color');
			$reverse_font_color= $this->setDefaultValue($user['reverse_font_color'],'reverse_font_color');
			$reverse_bg_color  = $this->setDefaultValue($user['reverse_bg_color'],'reverse_bg_color');
			$result = true;
			$i++;
	// 		print_r($user);
			$message .= 'user_id'.$user_id.' user_name'.$user_name;
		}

		if(!empty($user_id)){
	//		$message .= 'user_id'.$user_id.'user_name'.$user_name.'password'.$password.'def_name'. $def_name.'pg_name'.$pg_names.'print_id'. $print_id;;
			$_SESSION['user_id']   = $user_id;
			$_SESSION['user_name'] = $user_name;
			$_SESSION['password']  = $password;
			$_SESSION['def_name']  = $def_name;
			$_SESSION['pg_name']   = $pg_names;
			$_SESSION['print_id']  = $print_id;
			$_SESSION['font_size'] = $font_size;
			$_SESSION['font_color'] = $font_color;
			$_SESSION['bg_color']   = $bg_color ;
			$_SESSION['reverse_font_color'] = $reverse_font_color;
			$_SESSION['reverse_bg_color']   = $reverse_bg_color ;
		}else{
			$result = false;
		}



		if(!$result){

			$message .= ' ログインに失敗しました。';
		}

		return $result;
	}

}

////////////////////////////////////////////////////////////////////////////////共通関数
//エラー関数
function errOutPut($mess){
	//クラスのインスタンス化（ログ）
	$oLog      = New Log('');
	$oLog->info(__FILE__.':'.__LINE__.'[mess]:'.($mess));
	//エラーメッセージ受け渡し
	header('Location: ./sessDestroy.php?mess='.$mess);
	exit;
}
function sessionCheck(&$message){
	$checkKeyArray = array('user_id'
	,'user_name','password','def_name'
	//,'pg_name'                                 //pgname は配列なのでみないでおく
	,'print_id','font_size'
	,'font_color','bg_color','reverse_font_color'
	,'reverse_bg_color');
	foreach($checkKeyArray as $key){
		if(!array_key_exists($key,$_SESSION)){
			return false;
		}

		if(empty($_SESSION[$key])){
			return false;
		}
	}
	return true;
}

//実行ファイルの名前を指定してその名前を習得
//in:実行ファイルの名前(パス)
//out:その実行ファイルの名前(複数ある場合は先に見つかったもの)
function getExecTitle($strPs){
	$retVal='';

	return $retVal;
}

function changeHttps(){
	if (empty($_SERVER['HTTPS'])) {
		header("Location: https://{$_SERVER['HTTP_HOST']}{$_SERVER['REQUEST_URI']}");
		exit;
	}
}

//プロセス終了時の戻る対応
//プロセスIDが一致するプロセスの存在確認
function Unix_IsPidExisted($pid,$infname,$outfname,$log)
{
	$ret = false;
	//プロセスIDは繰り返し使用されるため
	//インプットファイルとアウトプットファイルも確認コマンドに含める
	$cmd = "/bin/ps -p " .$pid. " -o pid ";
	$fp  = popen($cmd, "r");
	while( ($line = fgets($fp)) != false ){
		if( intval(trim($line)) == $pid ){
			$ret = true;
			break;
		}
	}
	pclose($fp);
	return $ret;
}

//ステータスバー対応
//$pidを基準に子プロセスの状態（ジョブ名・ステータス・PID）取得
function getProcessIdStatus($pid,$log){

	$statArray = array(
		'pid'        => '',
		'stat'       => '',
		'jobname'    => '',
		'command'    => ''
	);

	$processTree = array();

	//子のプロセスをすべて取得
	getLastProcessIds($pid,$pid,$processTree,"children");

	//子のプロセスID（複数）子のプロセスIDのみにして取得
	$targetPids = array();
	foreach($processTree as $process){
		$tmpArray = split(",",$process);
		array_push($targetPids,$tmpArray[count($tmpArray)-1]);
	}

	//配列逆から調べる（子プロセスから調べる）
	//../exec/を見つけたら状態（ジョブ名・ステータス・PID）を取得
	//../job/を見つけたら無条件で（ジョブ名・ステータス・PID）を取得して終了
	$breakflg = 0;
	$countArray = count($targetPids);
	for ($i = $countArray-1; $i >= 0; $i--) {
		$process = shell_exec("ps aux |awk 'match($2,/". $targetPids[$i] ."/) {printf \"%s||%s||%s||%s||\",$2,$8,$11,$12 }' ");
		$setflg=0;
		if(!empty($process) && $process != "" ){
			$tmpArray = explode("||",$process);
			$loopcount = 0;
			foreach($tmpArray as $tmp){
				switch ($loopcount) {
					case 0:
						$statArray['pid']     = $tmp;
						break;
					case 1:
						if(preg_match("/T/",$tmp)){
							$tmp="P";
						}else{
							$tmp="A";
						}
						//一番子がどの状態かで判断
						if($statArray['stat'] === ""){
							$statArray['stat']    = $tmp;
						}
						break;
					case 2:
						if (preg_match("[^../exec/]", $tmp)) {
							$statArray['jobname'] = str_replace("../exec/", "", $tmp);
						}
						break;
					case 3:
						//「../job/」で取得できたときはこちらを使用する
						if($statArray['jobname'] != ""){
							break;
						}
						if (preg_match("[^../job/]", $tmp)) {
							$statArray['jobname'] = str_replace("../job/", "", $tmp);
							$statArray['jobname'] = strtoupper(substr($statArray['jobname'], 0,-3));
						}
						break;
				}
				$loopcount++;
				if($loopcount >= 4){
					break;
				}
			}
		}
	}
	return $statArray;
}

//ステータスバー対応
//再帰的に子プロセスのIDを取得
//      $argがchildrenのとき
//          戻り値はpidのみを返す
//          例）processTree[0]⇒123,457
//              processTree[1]⇒123,457,852
//              processTree[2]⇒123,745
//      $argがleafのとき
//          戻り値は葉のpidとその親のpidをカンマ繋ぎで返す
//          例）processTree[0]⇒123,457,852 ←葉の852
//              processTree[1]⇒123,745 ←葉の745
function getLastProcessIds($pid,$str,&$processTree,$arg){
	//子のプロセスID取得
	$tmppid = shell_exec("ps  --no-header --ppid " . $pid . " |awk '{printf \"%s\\n\",$1}' ");
	$tmppid = explode("\n",$tmppid);
	if(!empty($tmppid[0]) && $tmppid[0] != "" ){
		if($arg=="children"){
			array_push($processTree,$str.",".$tmppid[0]);
		}
		getLastProcessIds($tmppid[0], $str.",".$tmppid[0],$processTree,$arg );
	}else{
		//それ以上子が居ない時に通る
		if($arg=="leaf"){
			array_push($processTree, substr($str, (strrpos($str,',') + 1) ));
		}
	}
}
?>
