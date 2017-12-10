<?php
// FIXME デバッグ用ヘッダー。後で削除する。
header('Access-Control-Allow-Origin: *');

// セッションを利用するとリクエストに排他ロックがかかるのでSSEではセッションは利用しない。
// session_start();
$sani = $_GET;
$pid = $sani['pid'];
$infname = $sani['infname'];
$outfname = $sani['outfname'];

if (! empty($pid) && ! empty($infname) && ! empty($outfname)) {
	require_once ('./lib/config.php');
	require_once ('./lib/log.php');
	require_once ('./lib/clsAsynchronousProcess.php');
	require_once ('./lib/clsScreen.php');

	$oLog = new Log('');
	$clsAP = new AsynchronousProcess($infname, $outfname);
	$screen = new clsScreen();

	$acceptHeader = $_SERVER['HTTP_ACCEPT'];

	// Acceptヘッダーに合わせて処理を切り分ける。
	if ($acceptHeader === 'text/event-stream') {
		// SSE
		header('Content-Type: text/event-stream;charset=UTF-8');
		header('Cache-Control: no-cache');

		$lastRes = '';

// 		$i = 0;
		while (1) {
			list ($time, $res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);

			if (strcmp($res, $lastRes) === 0) {
				// 前回と同じ内容の場合は何も送らない。(接続が切断されないようにポーリングデータだけ送る)
				echo ":\n\n";
			} else {
				$lastRes = $res;

				// データのタイムスタンプを末尾に付与
				$res .= '<input type="hidden" id="dataTimestamp" value="' . $time . '" />';
				// FIXME Beep音確認用
// 				if ($i == 5) {
// 					$res .= '<input type="hidden" id="err-buz" value="10" />';
// 				}
				$res = "data: " . $res . "\n\n";
				$res = mb_convert_encoding($res, "UTF-8", "SJIS-WIN");

				$screen->screenParse($res);

// 				$i++;
			}

			ob_flush();
			flush();

			if ($is_break) {
				break;
			}

			// 100ミリ秒周期(マイクロ秒指定)
			// FIXME 定数化
			usleep(1 * 1000 * 100);
		}
	} else {
		// 通常のリクエスト
		// セッションは使わないがタイムアウトを防ぐために指定する
		session_start();

		list ($time, $res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);
		// データのタイムスタンプを末尾に付与
		$res .= '<input type="hidden" id="dataTimestamp" value="' . $time . '" />';
		$screen->screenParse($res);
	}
}

function get_response($oLog, $clsAP, $pid, $infname, $outfname)
{
	$is_break = false;

	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	$res = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">';
	list ($time, $data) = $clsAP->pReadAndTime();
	$res .= $data;

	// プロセス終了時の戻る対応
	// 指定のプロセスIDが実行中かチェック
	$state = Unix_IsPidExisted($pid, $infname, $outfname, $oLog);
	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	if ($state === false) {
		// 終了している場合は以下のinputを出力
		$res .= '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
		$is_break = true;
	} else {
		$is_break = false;
	}

	// SSEは連続する改行コードが終端文字として扱われるので、データ内の改行コードはすべて削除する。
	$res = preg_replace("/\r\n|\r|\n/", "", $res);

	return array(
		$time,
		$res,
		$is_break
	);
}
?>
