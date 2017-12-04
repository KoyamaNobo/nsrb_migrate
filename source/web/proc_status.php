<?php
//
// プログラム実行時の状態(表示の下側)を作って返す
// 2017/06/09 create koyama

// セッションを利用するとリクエストに排他ロックがかかるのでSSEではセッションは利用しない。
// session_start();
$sani = $_GET;

$pid = $sani['pid'];

if (! empty($pid)) {
	require_once ('./lib/config.php');
	require_once ('./lib/log.php');

	$oLog = new Log('');

	$acceptHeader = $_SERVER['HTTP_ACCEPT'];

	// Acceptヘッダーに合わせて処理を切り分ける。
	if ($acceptHeader === 'text/event-stream') {
		// SSE
		header('Content-Type: text/event-stream;charset=UTF-8');
		header('Cache-Control: no-cache');

		while (1) {
			list ($res, $is_break) = get_response($oLog, $pid);

			$res = "data: " . $res . "\n\n";
			$res = mb_convert_encoding($res, "UTF-8", "SJIS-WIN");

			echo $res;

			ob_flush();
			flush();

			if ($is_break) {
				break;
			}

			// 1秒周期(マイクロ秒指定)
			// FIXME 定数化
			usleep(1 * 1000 * 1000);
		}
	} else {
		// 通常のリクエスト
		list ($res, $is_break) = get_response($oLog, $pid);
		echo $res;
	}
}

function get_response($oLog, $pid)
{
	$is_break = false;

	// プロセスの状態取得
	// $oLog->info('microtime(true) = '. microtime(true) . ' ' . $sani['pid'] .__FILE__.__LINE__);
	$state = Unix_IsPidExisted($pid, '', '', $oLog);

	if ($state === false) {
		// プロセスが終了している場合は以下のinputを出力してループを終了する。
		$res = '<div class="status">';
		$res .= '	<input id="procesEnd" type="hidden" value="end">';
		$res .= '</div>';

		$is_break = true;
	} else {
		$statArray = getProcessIdStatus($pid, $oLog);

		$res = '<div class="status">';
		$res .= '	<input id="status2" type="hidden" class="statusGet" value="' . $statArray['stat'] . '" />';
		$res .= '	<input id="status4" type="hidden" class="statusGet" value="' . $statArray['jobname'] . '" />';
		$res .= '</div>';

		$is_break = false;
	}

	return array(
		$res,
		$is_break
	);
}
?>
