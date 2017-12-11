<?php
// FIXME デバッグ用ヘッダー。後で削除する。
header('Access-Control-Allow-Origin: *');

if(!empty($_POST)){
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/config.php');
	$sani = $_POST;
	if(!empty($sani['infname']) && !empty($sani['outfname'])){
		require_once('./lib/clsScreen.php');
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?php
	$clsAP = New AsynchronousProcess($sani['infname'],$sani['outfname']);
// 		$clsAP->oLog->info(__FILE__.':'.__LINE__.':Value '.print_r($sani,true));
	$oLog   = New Log('');
	$screen = New clsScreen();

	// データ書き込み＆処理されるまで待機
	if (is_array ($sani['value'])) {
		// パラメータが複数指定されている場合
		$count = count($sani['value']);
		for ($i = 0 ; $i < $count - 1; $i++) {
			$value = $sani['value'][$i];
			$clsAP->pWriteAndReadWait($value);
		}

		// 最後は出力結果が得られるまで待機
		$clsAP->pWriteAndProcWait($sani['value'][$count - 1]);
	} else {
		$clsAP->pWriteAndProcWait($sani['value']);
	}

	list($time, $data) =  $clsAP->pReadAndTime();

	//プロセス終了時の戻る対応（指定のプロセスIDが実行中かチェック）
	$state = Unix_IsPidExisted($sani['pid'],$sani['infname'],$sani['outfname'],$oLog);
	//指定のプロセスIDが実行中かチェック
	if($state==false){
		$data .= '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
	}
	// データのタイムスタンプを末尾に付与
	$data .= '<input type="hidden" id="dataTimestamp" value="' . $time . '" />';

	$screen->screenParse($data);

	}
}else{
	echo "Bad Request";
}
?>
