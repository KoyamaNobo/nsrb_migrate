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

		while (1) {
			list ($res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);

			if (strcmp($res, $lastRes) === 0) {
				// 前回と同じ内容の場合は何も送らない。(接続が切断されないようにポーリングデータだけ送る)
				$lastRes = $res;

				echo ":\n\n";
			} else {
				$lastRes = $res;

				$res = "data: " . $res . "\n\n";
				$res = mb_convert_encoding($res, "UTF-8", "SJIS-WIN");

				$screen->screenParse($res);
			}

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
		list ($res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);
		$screen->screenParse($res);
	}
}

function get_response($oLog, $clsAP, $pid, $infname, $outfname)
{
	$is_break = false;

	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	$res = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">';
	$res .= $clsAP->pRead();

	// プロセス終了時の戻る対応
	// 指定のプロセスIDが実行中かチェック
	$state = Unix_IsPidExisted($pid, $infname, $outfname, $oLog);
	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	if ($state === false) {
		// 終了している場合は以下のinputを出力
		$res .= '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
		// FIXME プロセスが終了した後も処理を継続する必要があるかどうか分からないので一旦処理を継続する。
		$is_break = false;
	} else {
		$is_break = false;
	}

	// SSEは連続する改行コードが終端文字として扱われるので、データ内の改行コードはすべて削除する。
	$res = preg_replace("/\r\n|\r|\n/", "", $res);

	return array(
		$res,
		$is_break
	);
}
?>
