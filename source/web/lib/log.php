<?php
//Multiline error log class
//ログローテートしていないので常時onにしない
Class Log {
  //
	private $errorFile =  'error.log';
	private $infoFile =  'info.log';
	private $logPath = '../log/';
	private $errorPath = '';
	private $infoPath  = '';
	private $defFilePath = '../XXXX.log';

	public function __construct($dirName){
// 		$this->errorPath = $dirName. '/' . $this->logPath . '/' .$this->errorFile;
// 		$this->infoPath = $dirName. '/' . $this->logPath . '/' .$this->infoFile;
		if($dirName !== ''){
			$this->logPath = $dirName;
		}
		$this->errorPath = $this->logPath . '/' .$this->errorFile;
		$this->infoPath  = $this->logPath . '/' .$this->infoFile;

		if(file_exists('./lib/config.php')){
			require_once('./lib/config.php');
		}

		//ディレクトリの存在がなければ作る
		if(!is_dir($this->logPath)){
			$msg='logdir '.$this->logPath.' '.is_dir($this->logPath);
			//msgを作りながらディレクトリ作成
			$msg.='create '.mkdir( $this->logPath , 0777).' : '.__FILE__.':'.__LINE__;
			shell_exec('logger -i "'.$msg.'"');
		}
		//エラー用のファイルがなければ作成
		if(!file_exists($this->errorPath)) {
			touch($this->errorPath);
			chmod($this->errorPath, 0777);
// 			$fp = fopen($this->errorPath,'w');
// 			fclose($fp);
		}
		//info用のファイルがなければ作成
		if(!file_exists($this->infoPath)) {
			touch($this->infoPath);
			chmod($this->infoPath, 0777);
// 			$fp = fopen($this->infoPath,'w');
// 			fclose($fp);
		}


// 		openlog("myScriptLog", LOG_PID | LOG_PERROR, LOG_LOCAL0);
	}

	private function detect_my_encoding($msg){
		return mb_detect_encoding($msg,"auto");
	}

	function set_utf8_encording($msg){
		if(preg_match('/SJIS/',$this->detect_my_encoding($msg))){
			return mb_convert_encoding($msg,'UTF-8','SJIS');
		}else{
			return $msg;
		}
	}

	function rem_line_break($string, $to = PHP_EOL){
		$string = preg_replace("/\r\n|\r|\n/", $to, $string);
		$string = preg_replace("/\n/", '[LF]', $string);
		return $string;
	}
	function rem_quote($string, $to = "'\""){
		$string = preg_replace("/[".$to."]/", '[quote]', $string);
		return $string;
	}

  /*    Info   */
	public function info($msg) {
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
// 		shell_exec('logger -i"log stat '.LOGSETFILE.__FILE__.__LINE__.' "');
	    if(defined('LOGSETFILE') && LOGSETFILE == true){
	    	$this->sysinfo($msg);
	    }else{
			$date = date('Y/m/d H:i:s');
			$log = "Date:".$date . " |INFO    |" .$msg." \n";
		    error_log($log, 3, $this->infoPath);
	    	$this->sysinfo($msg);
		}
	}
    /*   Error   */
	public function error($msg) {
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
	    if(defined('LOGSETFILE') && LOGSETFILE == true){
	    	$this->syserror($msg);
	    }else{
		    $date = date('Y:m:d H:i:s');
		    $log = "Date:".$date . " |ERROR   |" .$msg." \n";
		    error_log($log, 3, $this->errorPath);
	    	$this->syserror($msg);
		}
	}

	/*    Info   */
	public function teeInfo($filePeace,$msg) {
		$fileName = $this->defFilePath;
		$patern = 'XXXX';
		$replacement = $filePeace;
		$fileName = preg_replace($patern,$replacement,$fileName);
		//dateを一回にすることで日付は同一性が守られる?
		$date = date('Y:m:d H:i:s');
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
		$log = "Date:".$date . " |ERROR   |" .$msg." \n";
		error_log($log, 3, $fileName);
		syslog(LOG_INFO,$log);
	}

	/*   Error   */
	public function teeError($filePeace,$msg) {
		$fileName = $this->defFilePath;
		$patern = 'XXXX';
		$replacement = $filePeace;
		$fileName = preg_replace($patern,$replacement,$fileName);
		//dateを一回にすることで日付は同一性が守られる?
		$date = date('Y:m:d H:i:s');
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
		$log = "Date:".$date . " |ERROR   |" .$msg." \n";
		error_log($log, 3, $fileName);
		syslog(LOG_ERR,$log);
	}

	/*	dbError
	$sth=>errorInfo();用に作成 20130205
	*/
	public function dbError($e) {
	    $date = date('Y:m:d H:i:s');
	    $log = "Date:".$date . " |DB ERROR|[code]:" .$e[1]." [msg]:" .$e[2]." \n";
	    error_log($log, 3, $this->errorPath);
	}


	  /*  syslog Info
		syslog関数で出力用に作成 20130206
  */
	public function sysinfo($msg) {
	    $date = date('Y/m/d H:i:s');
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
	    $log = "Date:".$date . " |INFO    |" .$msg." \n";
	    syslog(LOG_INFO,$log);
	}
    /* syslog Error
		syslog関数で出力用に作成 20130206
      */
	public function syserror($msg) {
	    $date = date('Y:m:d H:i:s');
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
	    $log = "Date:".$date . " |ERROR   |" .$msg." \n";
	    syslog(LOG_ERR,$log);
	}

	/* dbError
		syslog関数で出力用に作成 20130206
	*/
	public function sysdbError($e) {
	    $date = date('Y:m:d H:i:s');
		//エンコードの変更
		$msg = $this->set_utf8_encording($msg);
		//改行コードの変更
		$msg = $this->rem_line_break($msg);
		$msg = $this->rem_quote($msg);
	    $log = "Date:".$date . " |DB Error|[code]:" .$e[1]." [msg]:" .$e[2]." \n";
	    syslog(LOG_WARNING,$log);
	    //エラーファイルにも書き出し
	}

	function __destruct() {
// 		closelog();
	}
}
?>
