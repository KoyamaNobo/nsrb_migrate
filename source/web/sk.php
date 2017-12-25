<?php
session_start();
require_once('./lib/config.php');
require_once('./lib/log.php');
require_once('./lib/clsSharedMemory.php');
$oLog = New Log('');
if(isset($_SESSION['user_id'])){
	if(!empty($_POST['value'])){
		//�v������(CTRL + F8)
		if(preg_match('/^C8$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -USR1 ' . $pid . ' ');
			}
		}
		//�Ɩ�����(CTRL + F9)
		if(preg_match('/^C9$/',$_POST['value'])){
			$pid = $_POST['pid'];
			if(is_numeric($pid)){

				$processTree = array();
				getLastProcessIds($pid, $pid, $processTree, 'leaf');
				// $oLog->info('pids: ' .print_r($processTree,true).__FILE__.':'.__LINE__);
				if (is_numeric($processTree[max(array_keys($processTree))])) {
					$oLog->info('pids: ' . $processTree[max(array_keys($processTree))] . __FILE__ . ':' . __LINE__);
					// ���f���ł��Ɩ������\�ɂ����邽�� CONT��TERM���s
					shell_exec('kill -CONT ' . $processTree[max(array_keys($processTree))] . ' ');
					shell_exec('kill -USR1 ' . $processTree[max(array_keys($processTree))] . ' ');
				}
				//���L��������j��
				SharedMemory::destroy($pid);
			}
		}
		//��~(CTRL + SHIFT + F4)
		if(preg_match('/^CS4$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -USR2 ' . $pid . ' ');
			}
		}
		//�ĊJ(CTRL + SHIFT + F5)
		if(preg_match('/^CS5$/',$_POST['value'])){
			$pid = getDescendantPid($_POST['pid']);
			if(is_numeric($pid)){
				shell_exec('kill -CONT ' . $pid . ' ');
			}
		}
// 		$oLog->info(__FILE__.':'.__LINE__.'process killer end');
	}
}

//�q����pid���擾
function getDescendantPid($pid){
	return getRecursiveChildPid($pid);
}

//�ċA�I�Ɏq����pid����葱����
//�q����pid���Ȃ��Ȃ�����I��
function getRecursiveChildPid($pid){
	$pidres = shell_exec("ps --ppid " . $pid . " |grep -v PID |grep -v tee |awk '{printf $1}'");
	if(!empty($pidres)){
		return getRecursiveChildPid($pidres);
	}else{
		//count��0�Ȃ�q�͂��Ȃ��Ƃ�������
		//���̂܂܂�pid��Ԃ�
		return $pid;
	}
}
?>
