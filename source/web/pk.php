<?php
session_start();
//valueのcheckはしない。ただし、inoutの両方のファイルが完全にあるプロセスのみを落とす
require_once('./lib/log.php');
$oLog = New Log('');
if(isset($_SESSION['user_id'])){
	if(!empty($_POST['infname']) && !empty($_POST['outfname'])){
		$cmd = 'ps aux|grep -v grep |grep ' . $_POST['outfname'] . " |awk '{printf \"%s||\",$2 }' ";
// 	$oLog->info('pids: ' .$cmd.':'.__FILE__.':'.__LINE__);
		$pids = shell_exec($cmd);
		$pidArray = explode('||',$pids);
		foreach($pidArray as $pid){
			if(!is_numeric($pid)){
				continue;
			}
			$ppid = "";
			//一番下が落ちると必ずすべて落ちるとは限らない
			for($count=0;$count < 3;$count++){
				$processTree = array();
				getLastProcessIds($pid,$pid,$processTree,'leaf');
	// 		$oLog->info('pids: ' .print_r($processTree,true).__FILE__.':'.__LINE__);
				if(is_numeric($processTree[max(array_keys($processTree))])){
	$oLog->info('pids: ' .$processTree[max(array_keys($processTree))].__FILE__.':'.__LINE__);
					//中断時でも業務放棄可能にさせるため　CONTとTERM発行
					shell_exec('kill -CONT ' . $processTree[max(array_keys($processTree))] . ' ');
					shell_exec('kill -USR1 ' . $processTree[max(array_keys($processTree))] . ' ');
				}
			}
		}
// 		$oLog->info(__FILE__.':'.__LINE__.'process killer end');
	}
}
?>
