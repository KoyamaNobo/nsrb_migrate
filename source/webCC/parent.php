<?php
define('CSCREEN','/clear\s+screen/i');
require_once('./lib/log.php');
if(isset($argv[1])){
	$cmd = $argv[1];
}else{
	exit;
}

//file削除のために両方必要
$fnameCtoP = $argv[(count($argv) - 1)];
$fnamePtoC = $argv[(count($argv) - 2)]; //書き込み用ファイル
$oLog = New Log('');
$efilename = DATA_SAVE_PASS.$argv[(count($argv) - 3)]."error.log"; 
$descriptorspec = array(
   0 => array("pipe", "r"),  // stdin は、子プロセスが読み込むパイプです。
   1 => array("pipe", "w"),  // stdout は、子プロセスが書き込むパイプです。
   2 => array("file", $efilename, "w") //エラーはファイルに書き込みます。
);

$strWrite = '' ;//入力データの格納
$strRead   = '' ;//output用データの格納
$readStatus =  '';

// echo 'test:'.$cmd." ";
// print_r($argv);

// エラーファイル作成
touch( $efilename );


$startTime = 0;
//unixタイムスタンプに変更(子プロセスができたときに開始時間+(EXEC_LIVE * 60)で初期化する)
$t = new clsProcessStruc;
$t->process = proc_open($cmd,$descriptorspec,$t->pipes);
//fgets等で固まってしまうため
stream_set_blocking($t->pipes[0],0);
stream_set_blocking($t->pipes[1],0);
if(is_resource($t->process)){
// 			stream_set_blocking($t->pipes[0],0);
// 			stream_set_blocking($t->pipes[1],0);
	
	$oLog->info("success".__FILE__.__LINE__);
	$startTime = time() + (EXEC_LIVE * 60);
	while(1){
		$exitWaitFlg = false;
		//最初に少し処理を待ってからスタート
		usleep(EXEC_SLEEP);
		
		//プロセスの状態がfalseの場合終了
		$procStatus = proc_get_status($t->process);

		//実行系への入力
		if(!file_exists($fnamePtoC)){
			$oLog->info(__FILE__.':'.__LINE__.'resource1:');
			//入力用のtmpファイルが存在しなければ強制終了
			break;
		}
		$fp = fopen($fnamePtoC,'r+');
		$wRes = 0;
		if($fp){
			stream_set_blocking($t->pipes[0],1);
			$strWrite = fgets($fp,1024);
			if(!empty($strWrite)){
				fflush($t->pipes[0]);
				$wRes = fwrite($t->pipes[0],$strWrite);
				//決め打ちで入力したら次のreadまで少し待つ
				usleep(500);
			}
			if($wRes != 0){
				ftruncate($fp,0);
			}else{
			}
			unset($wRes);
			fclose($fp);
		}else{
			//ファイルが正常に開けないときは強制終了
			$oLog->info(__FILE__.':'.__LINE__.' file:open error');
			break;
		}
		
		//実行系からの対しての出力
		//実行系の結果が残りどれくらいあるかのステータスを取得
		//残りの読み込みデータがなければ読み込まない
//  		$oLog->info(__FILE__.':'.__LINE__.'readed:'.$startTime);
		while(!empty($strRead = fgets($t->pipes[1],1024))){
			//改行のみは出力しない
			if(strlen($strRead) > 0){
				echo $strRead;
				//改行は出力に含まれているため不要 comment koyama 20150713
// 				echo $strRead.PHP_EOL;
			}
		}
		//終了予定時間を過ぎたら強制終了
		if($startTime < time()){
			break;
		}
		
		//プロセス終了時の戻る対応
		//プロセスの状態がfalseの場合終了
		if(!$procStatus["running"]){
				$exitWaitFlg = true;
				//少し待ってやらないとエラーメッセージを取り逃す
				usleep(EXEC_SLEEP);
		}
		//上のifを1回以上通っていたら
		if($exitWaitFlg === true){
			break;
		}
	}
	proc_close($t->process);
	//作成されたプロセス管理用のファイルは作ったレベルで処理(comment koyama 20150707)
// 	unlink($fnameCtoP);
// 	unlink($fnamePtoC);
}



//プロセスの管理用のクラス
//author koyama
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