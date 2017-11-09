#! /usr/bin/php
<?php
#プログラムの実行の親となる処理
define('CSCREEN','/clear\s+screen/i');
require_once('./lib/config.php');
require_once('./lib/log.php');
require_once('./lib/clsScreenToHTML.php');

if(isset($argv[1])){
	$splited_cmd = preg_split('/\s+/',$argv[1]);
echo "print_r [".print_r($splited_cmd,true)."]".__FILE__.__LINE__.PHP_EOL;
	$cmd         = array_shift($splited_cmd);
echo "print_r [".print_r($splited_cmd,true)."]".__FILE__.__LINE__.PHP_EOL;
	$option      = implode(" ", $splited_cmd);
}else{
	exit;
}

$startTime = 0;
#unixタイムスタンプに変更(子プロセスができたときに開始時間+(EXEC_LIVE * 60)で初期化する)
$oLog    = New Log('');

echo "procstart [".$cmd."]"."[".$option."]".__FILE__.__LINE__.PHP_EOL;
$oLog->info("procstart [".$cmd."]".__FILE__.__LINE__);
$t = new clsProcessStruc;
$t->process = pcntl_exec($cmd,$option);
if($t->process != FALSE){
	pcntl_wait($status);
echo "print_r [".print_r($status,true)."]".__FILE__.__LINE__.PHP_EOL;
	while(1){
		if(pcntl_wifexited($status) == TRUE){
			break;
		}
	}
}else{
	echo "error [".pcntl_strerror(pcntl_get_last_error())."]".__FILE__.__LINE__.PHP_EOL;
}


#プロセスの管理用のクラス
#author koyama
class clsProcessStruc {
	public $process;
	public $pipes;

	function __construct(){
		$this->process = 0;
		$this->pipes = (array)NULL;
	}

	function getAlive(){
		var_dump($this);
	}
}
?>