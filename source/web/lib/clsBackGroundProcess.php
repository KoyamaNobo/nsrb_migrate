<?php
//
//php起動用
//
require_once('./lib/log.php');
require_once('./lib/clsSharedMemory.php');
class BackgroundProcess{
	public $strCmd        = '';
	public $return_val    = array();
	public $output        = array();
	public $getline_index = 0;
	public $pid           = 0;
	public $tempnameOut   = '';//起動した先の標準出力に
	public $tempnameIn    = '';//起動した先の標準入力に
	public $oLog;
	public $memory;

	function __construct($cmd,$id){
		$this->oLog = New Log('');
		$this->getline_index = 0;
		$error_path = getenv('ERROR_PATH');
// 		$this->tempnameOut = tempnam($error_path,TEMP_FILE_PREFIX);
// 		$this->tempnameIn  = tempnam($error_path,TEMP_FILE_PREFIX);
		// 元々はファイルを使ってプロセス間通信をしていたが共有メモリで行う仕組みに変更したためファイルは不要になった。
		// ただ、プロセスを特定するのにファイルパスを利用しているためファイルパスのパラメータはそのまま残して、ファイルの実体だけを作らないようにしている。
		$this->tempnameOut = realpath($error_path) . '/' . TEMP_FILE_PREFIX . bin2hex(openssl_random_pseudo_bytes(4));
		$this->tempnameIn = realpath($error_path) . '/' . TEMP_FILE_PREFIX . bin2hex(openssl_random_pseudo_bytes(4));
		$error_file = $error_path . $id .bin2hex(openssl_random_pseudo_bytes(4));
		// $this->oLog->info('ERROR_PATH:'.$error_path . $id);
		touch($error_file);
		chmod($error_file,0777);
		putenv('ERROR_EXEC_PATH='.$error_file);
		//TODO:20170206 LMのときは個々のパターンを変更
		shell_exec('logger -i " '.$cmd.' '.preg_match('/sh/',$cmd).' '.__LINE__.' "');
		if(preg_match('/job/',$cmd) == TRUE){
			$cmd = $cmd.' '.$id.' 2> '.$error_file.' ';
		}else{
			$cmd = $cmd.' '.$id.' 2>&1 |tee '.$error_file.' ';
		}

		//前のエラーが残っていたら消す。ファイルが開けないか丸めに失敗したらエラー
		$truncateFp = fopen($error_path . $id,'w');
		if(!$truncateFp && !ftruncate( $truncateFp , 0 )){
			$this->oLog->error(__FILE__.':'.__LINE__.':tempOut File cant Created');
		}

// 		if(!touch( $this->tempnameOut )){
// 			$this->oLog->error(__FILE__.':'.__LINE__.':tempOut File cant Created');
// 		}
// 		if(!touch( $this->tempnameIn)){
// 			$this->oLog->error(__FILE__.':'.__LINE__.':tempIN File cant Created');
// 		}
// 		if(!chmod( $this->tempnameOut,0777)){
// 			$this->oLog->error(__FILE__.':'.__LINE__.':tempIN File cahnge Created');
// 		}
// 		if(!chmod( $this->tempnameIn,0777)){
// 			$this->oLog->error(__FILE__.':'.__LINE__.':tempIN File cahnge Created');
// 		}
		//仕様変更のためrunExecに変更 20151006
// 		$this->strCmd = "php parent.php '".$cmd."' '".$id."' '".$this->tempnameIn."' '".$this->tempnameOut."' > ".$this->tempnameOut." & echo $!";
//		$this->strCmd = "php runExec.php '".$cmd."' '".$id."' '".$this->tempnameIn."' '".$this->tempnameOut."' > ".$this->tempnameOut." & echo $!";
		$this->strCmd = "php runExec.php '".$cmd."' '".$id."' '".$this->tempnameIn."' '".$this->tempnameOut."' > /dev/null & echo $!";
// 		$this->oLog->info(__FILE__.':'.__LINE__.':'.$this->strCmd);
	}

	//実行ファイルの実行
	function run(){
		exec($this->strCmd,$this->output,$this->return_val);
		$this->pid = $this->output[0];
	}

	//pidの値(num)を返す
	//pidがセットされていなければ-1
	//未使用
	function getPid(){
		if($this->pid !== 0){
			return $this->pid;
		}else{
			return -1;
		}
	}

	//未使用
	function getStatus(){
		return $this->return_val;
	}

	//Asyncと構造が違うのを吸収
	function getTempNameOut(){
		return $this->tempnameOut;
	}

	//Asyncと構造が違うのを吸収
	function getTempNameIn(){
		return $this->tempnameIn;
	}

	//対象からの読み込み
	function pRead(){
		$result =  '';

		if ($this->createSharedMemory()) {
			list($time, $result) = $this->memory->read_outputfile();
			$this->getline_index = substr_count($result, "\n");
		}

		return $result;
	}

	//対象からの読み込み
	function pReadAndTime(){
		if ($this->createSharedMemory()) {
			list($time, $result) = $this->memory->read_outputfile();
			$this->getline_index = substr_count($result, "\n");

			return array($time, $result);
		} else {
			return array(microtime(true), '');
		}
	}

	//対象への書き込み
	function pWrite($inputData){
		if(!is_null($inputData)){
			echo "p:write_data".$this_tempnameIn.PHP_EOL;

			if ($this->createSharedMemory()) {
				$this->memory->write_inputfile($inputData);
			}
		}
	}

	// 共有メモリを作成する
	function createSharedMemory()
	{
		if (empty($this->memory)) {
			// PIDをキーに共有メモリを作成する。
			$this->memory = new SharedMemory();
			$this->memory->open($this->pid);
		}

		return !$this->memory->is_error();
	}

	function __destruct(){
		$this->getline_index = 0;
		$this->pid           = 0;
	}
}

?>
