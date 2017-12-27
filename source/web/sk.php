<?php
session_start();
require_once('./lib/config.php');
require_once('./lib/log.php');
require_once('./lib/clsSharedMemory.php');
$oLog = New Log('');
if(isset($_SESSION['user_id'])){
	if(!empty($_POST['value'])){
		//プロ放棄(CTRL + F8)
		if(preg_match('/^C8$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -USR1 ' . $pid . ' ');
			}
		}
		//業務放棄(CTRL + F9)
		if(preg_match('/^C9$/',$_POST['value'])){
			$pid = $_POST['pid'];
			if(is_numeric($pid)){

				// 親プロセスから順に関連するプロセスをすべて落とす
				$processTree = array();
				getChildProcessIds($pid,$pid,$processTree);
				if(empty($processTree)){
					// 子プロセスが取得できなくても必ず親プロセスは指定する。
					array_push($processTree, $pid);
				}

				$killPids = explode(",",$processTree[count($processTree) - 1]);
				for($i = 0; $i < count($killPids); $i++){
					$oLog->info('pids: ' . $killPids[$i] . __FILE__ . ':' . __LINE__);
					// 中断時でも業務放棄可能にさせるため CONTとTERM発行
					shell_exec('kill -CONT ' . $killPids[$i] . ' ');
					shell_exec('kill -USR1 ' . $killPids[$i] . ' ');
				}

				//共有メモリを破棄
				SharedMemory::destroy($pid);
			}
		}
		//停止(CTRL + SHIFT + F4)
		if(preg_match('/^CS4$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -USR2 ' . $pid . ' ');
			}
		}
		//再開(CTRL + SHIFT + F5)
		if(preg_match('/^CS5$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -CONT ' . $pid . ' ');
			}
		}
// 		$oLog->info(__FILE__.':'.__LINE__.'process killer end');
	}
}

//子孫のpidを取得
function getDescendantPid($pid){
	return getRecursiveChildPid($pid);
}

//再帰的に子供のpidを取り続ける
//子供のpidがなくなったら終了
function getRecursiveChildPid($pid){
	$pidres = shell_exec("ps --ppid " . $pid . " |grep -v PID |grep -v tee |awk '{printf $1}'");
	if(!empty($pidres)){
		return getRecursiveChildPid($pidres);
	}else{
		//countが0なら子はいないということ
		//そのままのpidを返す
		return $pid;
	}
}
?>
