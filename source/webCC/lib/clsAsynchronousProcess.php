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

	function __construct($infname,$outfname){
		require_once('./lib/log.php');
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
		$result = true;
		if(!empty($inputData)){
			if(mb_detect_encoding($inputData,$this->charCode) !== CHARSET){
				$encode = mb_detect_encoding($inputData,$this->charCode);
				$inputData = mb_convert_encoding($inputData,CHARSET,$encode);
			}
			$fp = fopen($this->fnamePtoC,'w');
			if($fp){
				stream_set_blocking($fp,0);
				$result = fwrite($fp,$inputData.PHP_EOL);
				if($result > 0){
				}
				fclose($fp);
			}
		}
		return $result;
	}

	//対象への読み込み
	//今なん行目まで読んでいるかを基準に一度読んだところは読み飛ばす
	function pRead(){
		$result = '';
		$filesize = 0;  //
		$counter = 0;
		$iPreFileSize = 0;
		$readCount = READ_COUNT;    //何度読み込みをスキップするか(スキップだけをして処理中かどうかを判定しない)
		$ii = 0;
		// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);

		while(1){
			if(file_exists($this->fnameCtoP) == false){
				break;
			}
			clearstatcache(true,$this->fnameCtoP);
			if( filesize($this->fnameCtoP) !== 0 ){
				$readCount--;
			}else {
// 				$iPreFileSize = filesize($this->fnameCtoP);
				//書き込み中
// 				usleep(1);
			}

			if( $readCount == 0){
				$fp = fopen($this->fnameCtoP,'r');
				if($fp == false|| !flock($fp, LOCK_EX)){
					break;
				}
				stream_set_blocking($fp,0);
				$temp = '';
				while($temp = fgets($fp,2048)){
					//0行目にしないために先にインクリメント
					$ii++;
					$result .= $temp."\n";
					$this->getline_index = $ii;
				}
				fclose($fp);
				break;
			}
			$filesize = filesize($this->fnameCtoP);
			$counter++;
			//まわりすぎたら終了
			if($counter > $this->max_count){
				break;
			}
		}

		// $this->oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
		return $result;
	}


	function __destruct(){
		$this->getline_index = 0;
		$this->pid           = 0;
	}
}
?>
