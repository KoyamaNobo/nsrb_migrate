#!/usr/bin/php
<?php
require_once('./lib/log.php');
$oLog          = New Log(''); 
$tmpName = 'WK0768';
$spltArgArray = array();    //スプリットされた
//パラメータをスプリット
if(isset($argv[1])){
	$spltArgArray = preg_split('/__/',$argv[1]);
	//引数を変換
	$nfcnv_arg = $spltArgArray[0];
	if(isset($spltArgArray[1]) && !empty($spltArgArray[1])){
		//FLCNVを実行
		//引数を変換
		$flcnv_arg = $spltArgArray[1];
		$flcnv_arg = ' IDE=MSD MOD=CRE OFI='.$tmpName .' '. $flcnv_arg;
		$oLog->info('file:'.__FILE__.' command:'.'FLCNV \''.$flcnv_arg.'\' ' );
		exec('FLCNV \''.$flcnv_arg.'\' ',$output);
		foreach($output as $line){
			echo $line.PHP_EOL;
		}
		
		$pattern   = '/(PA3=[^\s]+)/';
		$replacement = 'PA3='.$tmpName;
		$nfcnv_arg = preg_replace($pattern,$replacement,$nfcnv_arg);
	}
	$oLog->info('file:'.__FILE__.' command:'.'NFCNV \''.$nfcnv_arg.'\'  2>&1' );
	exec('NFCNV \''.$nfcnv_arg.'\' 2>&1 ',$output);
	foreach($output as $line){
		echo $line.PHP_EOL;
	}
	$oLog->info('file:'.__FILE__.' INFO:'.'DataUtil2CSV.php-END' );
}
?>
