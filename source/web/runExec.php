<?php
#プログラムの実行の親となる処理
define('CSCREEN','/clear\s+screen/i');
require_once('./lib/config.php');
require_once('./lib/log.php');
require_once('./lib/clsScreenToHTML.php');
require_once('./lib/clsSharedMemory.php');

if(isset($argv[1])){
	$cmd = $argv[1];
}else{
	exit;
}

$efilename = DATA_SAVE_PASS.$argv[(count($argv) - 3)]."error.log";
$descriptorspec = array(
   0 => array("pipe", "r"),  # stdin は、子プロセスが読み込むパイプです。
   1 => array("pipe", "w"),  # stdout は、子プロセスが書き込むパイプです。
   2 => array("file", $efilename, "w") #エラーはファイルに書き込みます。
);

$endTime = 0;
#unixタイムスタンプに変更(子プロセスができたときに開始時間+(EXEC_LIVE * 60)で初期化する)
$oLog    = New Log('');

# エラーファイル作成
if(file_exists(preg_replace('/[^\/]+\.[^.]+$/','',$efilename))){
	touch( $efilename );
	chmod($efilename, 0777);
}else{
	#内容を変更するので個別
	mkdir(preg_replace('/[^\/]+\.[^.]+$/','',$efilename));
	touch( $efilename );
	chmod($efilename, 0777);
}
$oLog->info("procstart [".$cmd."]".__FILE__.__LINE__);
$t = new clsProcessStruc;
$t->process = proc_open($cmd,$descriptorspec,$t->pipes);
#fgets等で固まってしまうため
stream_set_blocking($t->pipes[0],0);
stream_set_blocking($t->pipes[1],0);
if(is_resource($t->process)){
	// stream_set_block1ing($t->pipes[0],0);
	// stream_set_blocking($t->pipes[1],0);
	#file削除のために両方必要
	$fnameCtoP = $argv[(count($argv) - 1)];
	$fnamePtoC = $argv[(count($argv) - 2)]; #書き込み用ファイル
	# TODO:$toHTMLにどれくらいの負荷がかかっているか調査をする必要がある
	$toHTML  = New clsScreenToHTML();
	$strRead = '' ;#output用データの格納

	$memory = new SharedMemory();
	// 自プロセスIDをキーに共有メモリを作成(処理終了時にはメモリを破棄)
	$id = getmypid();
	$memory->create($id);

	$oLog->info("success ::".$cmd."::".__FILE__.__LINE__);
	$startTime = time();
	$endTime = time() + (EXEC_LIVE * 60);
	$html = '';

	while(1){
		$exitWaitFlg = false;
		#最初に少し処理を待ってからスタート
		usleep(EXEC_SLEEP);

		#プロセスの状態がfalseの場合終了
		$procStatus = proc_get_status($t->process);

		// 実行系からの対しての出力
		// 実行系の結果が残りどれくらいあるかのステータスを取得
		// 残りの読み込みデータがなければ読み込まない
		// $oLog->info(__FILE__.':'.__LINE__.'readed:'.$endTime);
		$ii = 0;
		while(!empty($strRead = fgets($t->pipes[1],1024))){
			#改行のみは出力しない
			if(strlen($strRead) > 0){
				//  $oLog->info('readed:'.$strRead.__FILE__.':'.__LINE__,0);
				$toHTML->screen->elemParse($strRead);
			}
			$ii++;
		}
		if($ii > 0){
			$toHTML->lineIndex += $ii;
			$newHtml = $toHTML->getHtml();
			if (strcmp($html, $newHtml) !== 0) {
				$memory->write_outputfile($newHtml);
				$html = $newHtml;
			}
		}
		#終了予定時間を過ぎたら強制終了
		if($endTime < time()){
			break;
		}

		$wRes = 0;
		$strWrite = '' ;#入力データの格納
		stream_set_blocking($t->pipes[0],1);
		list($writeTime, $strWrite) = $memory->read_inputfile();
		if(!empty($strWrite)){
			fflush($t->pipes[0]);
			// $oLog->info("write [".$strWrite."]".__FILE__.__LINE__);
			$wRes = fwrite($t->pipes[0],$strWrite);
		}
		if($wRes != 0){
			$memory->delete_inputfile();
		}
		unset($wRes);

		#プロセス終了時の戻る対応
		#プロセスの状態がfalseの場合終了
		if(!$procStatus["running"]){
				$exitWaitFlg = true;
				#少し待ってやらないと外側がエラーメッセージを取り逃す
				#書き込んだデータが読み取られるまで待機。
				while (time() - $startTime <= EXEC_MAX_END_WAIT) {
					if ($memory->is_read_outputfile()) {
						break;
					}
					// getOut.phpで読みだされる周期でsleep
					usleep(SSE_GETOUT_SLEEP);
				}
		}
		#上のifを1回以上通っていたら
		if($exitWaitFlg === true){
			$oLog->info('exit process:'.$cmd.':exitcode:'.$procStatus['exitcode'].__FILE__.':'.__LINE__);
			break;
		}
	}
	// fclose($t->pipes[2]);
	// fclose($t->pipes[1]);
	// fclose($t->pipes[0]);
	if(preg_match('/\.sh/',$cmd)){
		proc_terminate($t->process);
	}else{
		//COBOL直接のときはUSR1
		proc_terminate($t->process,30);
	}
	#作成されたプロセス管理用のファイルは作ったレベルで処理(comment koyama 20150707)
# 	unlink($fnameCtoP);
# 	unlink($fnamePtoC);
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
