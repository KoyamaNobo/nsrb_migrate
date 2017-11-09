<?php
//
//プログラム実行時の状態(表示の下側)を作って返す
//2017/06/09 create koyama
require_once('./lib/log.php');
session_start();
if(!empty($_POST)){
	require_once('./lib/clsAsynchronousProcess.php');
	require_once('./lib/config.php');
	require_once('./lib/clsScreen.php');
	require_once('./lib/log.php');
    $oLog   = New Log('');
    $sani = $_POST;
    //プロセスの状態取得
    // $oLog->info('microtime(true) = '. microtime(true) . ' ' . $sani['pid'] .__FILE__.__LINE__);
    $state = Unix_IsPidExisted($sani['pid'],'','',$oLog);
    if($state==false){
        //終了している場合は以下のinputを出力
        echo '<div class="status">';
        echo '  <input id="procesEnd" type="hidden" value="end">';
        echo '</div>';
        exit(1);
    }
	$statArray = getProcessIdStatus($sani['pid'],$oLog);
}
?>
<div class="status">
    <!-- version 0.0.1 -->
 	<input id="status2" type="hidden" class="statusGet" value="<?php echo $statArray['stat'];?>" />
    <input id="status4" type="hidden" class="statusGet" value="<?php echo $statArray['jobname'];?>" />
</div>
<?php
?>
