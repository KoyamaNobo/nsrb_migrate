<?php
//
//php途中入出力用
//
class AsynchronousProcess{
	public $strCmd        = '';
	public $return_val    = array();
	public $output        = array();
	public $getline_index = 0;
	public $pid           = 0;
	public $fnameCtoP   = '';//起動した先の標準出力に
	public $fnamePtoC    = '';//起動した先の標準入力に
	public $max_count     = LOOP_COUNT; //readの時にループが限界に来たら
	public $oLog;
	public $charCode;
	public $memory;

	function __construct($infname,$outfname){
		require_once('./lib/log.php');
		require_once('./lib/clsSharedMemory.php');
		$this->oLog = New Log('');
		// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
		$this->getline_index = 0;
		$this->fnamePtoC = $infname;
		$this->fnameCtoP = $outfname;
		$this->charCode  = array('UTF-8','JIS', 'eucjp-win', 'SJIS');
		// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	}

	function run(){
		exec($this->strCmd,$this->output,$this->return_val);
		$this->pid = $this->output[0];
	}

	//pidの値(num)を返す
	//pidがセットされていなければ-1
	function getPid(){
		if($this->pid !== 0){
			return $this->pid;
		}else{
			return -1;
		}
	}

	//持っている情報からpidを特定
	function setPid(){
		$cmd = '';
		$cmd .= 'ps aux';
		if(!empty($this->fnameCtoP)){
			$cmd .= '|grep '.$this->fnameCtoP.' ';
		}
		if(!empty($this->fnamePtoC)){
			$cmd .= '|grep '.$this->fnamePtoC.' ';
		}
		$cmd .= "|grep -v grep |awk '{print $2}'";
		$retVal = shell_exec($cmd);
		//空文字になると困るのでintに変換することで0にする
		$this->pid = intval(explode(PHP_EOL,$retVal)[0]);
		return $this->pid;
	}
	function getStatus(){
		return $this->return_val;
	}


	function getTempNameOut(){
		return $this->fnameCtoP;
	}

	function getTempNameIn(){
		return $this->fnamePtoC;
	}
	//対象への書き込み
	function pWrite($inputData){
		if ($this->createSharedMemory()) {
			if(!empty($inputData)){
				if (mb_detect_encoding($inputData, $this->charCode) !== CHARSET) {
					$encode = mb_detect_encoding($inputData, $this->charCode);
					$inputData = mb_convert_encoding($inputData, CHARSET, $encode);
				}
				return $this->memory->write_inputfile($inputData.PHP_EOL);
			}

			return true;
		}
		return false;
	}

	//データを書き込み、対象データが処理されるまで待機する
	function pWriteAndProcWait($inputData){
		if ($this->createSharedMemory()) {
			list ($beforeTime, $beforeData) = $this->pRead();

			$this->pWrite($inputData);

			// まずはデータが読み取られるまで待機(最大500回。200マイクロ秒のループでも最大500回で100ミリ秒)
			for ($i = 0; $i < 500; $i++) {
				list($time, $result) = $this->memory->read_inputfile();
				if ($result === false) {
					break;
				}
				usleep(EXEC_SLEEP);
			}
			// データが処理されるまで待機(最大2000回。200マイクロ秒のループでも最大2000回で400ミリ秒)
			for ($i = 0; $i < 1000; $i++) {
				list ($time, $data) = $this->pRead();

				if (strcmp($beforeData, $data) !== 0) {
					break;
				}
				usleep(EXEC_SLEEP);
			}
		}
	}

	//データを書き込み、対象データが読み込まれるまで待機する
	function pWriteAndReadWait($inputData){
		if ($this->createSharedMemory()) {
			$this->pWrite($inputData);

			// まずはデータが読み取られるまで待機(最大1000回。200マイクロ秒のループでも最大1000回で200ミリ秒)
			for ($i = 0; $i < 1000; $i++) {
				list($time, $result) = $this->memory->read_inputfile();
				if ($result === false) {
					break;
				}
				usleep(EXEC_SLEEP);
			}
		}
	}


	//対象への読み込み
	//今なん行目まで読んでいるかを基準に一度読んだところは読み飛ばす
	function pRead(){
		$result = '';

		if ($this->createSharedMemory()) {
			list($time, $result) = $this->memory->read_outputfile();
			$this->getline_index = substr_count($result, "\n");
		}

		return $result;
	}

	//対象への読み込み
	//今なん行目まで読んでいるかを基準に一度読んだところは読み飛ばす
	function pReadAndTime(){
		if ($this->createSharedMemory()) {
			list($time, $result) = $this->memory->read_outputfile();
			$this->getline_index = substr_count($result, "\n");

			return array($time, $result);
		} else {
			return array(microtime(true), '');
		}
	}

	// 共有メモリを作成する
	function createSharedMemory()
	{
		if (empty($this->memory)) {
			if ($this->getPid() === - 1) {
				$this->setPid();
			}
			// PIDをキーに共有メモリを作成する。
			$this->memory = new SharedMemory();
			$this->memory->open($this->getPid());
		}

		return !$this->memory->is_error();
	}


	function __destruct(){
		$this->getline_index = 0;
		$this->pid           = 0;
	}
}
?>
