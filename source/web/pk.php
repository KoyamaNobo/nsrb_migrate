<?php
session_start();
//valueのcheckはしない。ただし、inoutの両方のファイルが完全にあるプロセスのみを落とす
require_once('./lib/log.php');
require_once('./lib/clsSharedMemory.php');
$oLog = New Log('');
if(isset($_SESSION['user_id'])){
	if(!empty($_POST['infname']) && !empty($_POST['outfname'])){
		$cmd = 'ps aux|grep -v grep |grep ' . $_POST['outfname'] . " |awk '{printf \"%s||\",$2 }' ";
// 	$oLog->info('pids: ' .$cmd.':'.__FILE__.':'.__LINE__);
		$pids = shell_exec($cmd);
		$pidArray = explode('||',$pids);
		$ppid = "";
		foreach($pidArray as $pid){
			if(!is_numeric($pid)){
				continue;
			}
			if(empty($ppid)){
				$ppid = $pid;
			}

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
// 		$oLog->info(__FILE__.':'.__LINE__.'process killer end');
	}
}
?>
