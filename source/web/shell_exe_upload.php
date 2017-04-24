<?php
//成功時
//　DataUtil2CSV.phpをバックグラウンドで実行させる
//　プログラム成功時成功時はPID:xxxxxを返す
//失敗時
//　引数なしはERR9011を返す

require_once(dirname(__FILE__).'/lib/config.php');
require_once(dirname(__FILE__).'/lib/log.php');


//ランダムの文字列作成
function makeRandStr($length) {
    $str = array_merge(range('a', 'z'), range('0', '9'), range('A', 'Z'));
    $r_str = null;
    for ($i = 0; $i < $length; $i++) {
        $r_str .= $str[rand(0, count($str))];
    }
    return $r_str;
}

//POSTの引数が存在しているか
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['word']) && !empty($_POST['word']) ){
		$strInp = $_POST['word'];
	}else{
        echo('ERR9011');
        exit();
	}
}

$oLog = New Log(''); 
$err_flg = "0";
$check_flg = "0";
$file_name = "";
set_time_limit(0);

//$strInp = "PA5=(1,4,C,'')^(5,3,C,'')^(8,26,J,'')^(60,20,J,'')^(100,20,J,'')^(140,8,C,'')^(148,14,C,'')^(162,2,N,'')^(164,1,N,'')^(165,1,N,'')^(166,1,N,'')^(167,1,N,'')^(168,1,N,'')^(169,9,C,'')^(178,7,C,'')^(185,2,C,'')^(187,6,C,'') PB7=C,C,J,J,J,C,C,S,S,S,S,S,S,C,C,C,C MN2=DA PA3=TCM PB1=../tmp/TCM.CSV PAA=NEW";

//「’」で囲まれた部分（項目名）を全て取り除く
$tmp = explode("'", $strInp);

$strInpNonDispName = "";
$count = 0;
$dispName = array();
foreach ($tmp as $value) {
    if($count % 2 == 0){
        $strInpNonDispName = $strInpNonDispName . $value;
    }
    else{
        //項目名はほかの変数へ格納
        array_push($dispName,$value);
    }
    $count = $count + 1;
}

//正しいNFCNVに渡せるパラメータに変換
//  例）
//    (1,4,C,'')→C4
$strPA5 = "PA5=";
$count = 0;
$strInpNonDispNameArray = explode(" ", $strInpNonDispName);
foreach ($strInpNonDispNameArray as $value) {
    $res = ereg("PA5=", $value); 
    if($res){
        // パラメータの中にPA5を削除
        $tmp = str_replace("PA5=", "", $value);
        // パラメータの中をタブで分割
        $tmpArry = explode("^", $tmp);
        foreach ($tmpArry as $dispItem) {
            // カンマで分割
            $dispItemArry = explode(",", $dispItem);
            if ($count != 0){
                $strPA5 = $strPA5 . ",";
            }
            if(count($dispItemArry) == 5){
                $strPA5 = $strPA5 . substr($dispItemArry[3], 0, 1) . (string)((int)$dispItemArry[1] + (int)$dispItemArry[2]);
            }
            else{
                $strPA5 = $strPA5 . substr($dispItemArry[2], 0, 1) . $dispItemArry[1];
            }
            $count = $count + 1;
        }
    }
}

//パラメータ変換したものを再結合してFNCNV用にパラメータ作成
$strFNCNVsend = "";
$count = 0;
$tmpArry = explode(" ", $strInpNonDispName);
foreach ($tmpArry as $value) {
    if ($count == 0){
        $strFNCNVsend = $strFNCNVsend . $strPA5;
    }else{
        $strFNCNVsend = $strFNCNVsend ." " .  $value;
    }
    $count = $count + 1;
}

//PB1=../tmp/の文字列を探してファイル名取得
$tmpArry = explode(" ", $strInpNonDispName);
foreach ($tmpArry as $value) {
    if (preg_match("[^PB1=../tmp/]", $value)) {
        $file_name = substr($value, 11);
    }
}

//バックグラウンドで起動させるプログラム（DataUtil2CSV.php）の出力先決定
$outputfile = "../tmp/" . $file_name . makeRandStr(5);

//出力先決定が既に存在しているかチェック
//存在している場合は出力先を変える
while(file_exists($outputfile)){
	$outputfile = "../tmp/" . $file_name . makeRandStr(5);
}

//出力先ファイル作成
if(!touch( $outputfile)){
    echo('ERR9017');
    exit();
}
chmod($outputfile,0777);

//出力される前に作っておく？
if(!file_exists($outputfile)){
	touch($outputfile);
	chmod($outputfile,0777);
}

//バックグラウンドでジョブ実行
$output     = array();
$cmd = "php ./DataUtil2CSV.php '".$strFNCNVsend."' >".$outputfile." 2>&1 & echo $! ";
$oLog->info('file:'.__FILE__.' command:'. $cmd );
exec($cmd,$output);

//戻り値としてプロセスIDとアウトプットファイル名を返す
echo "PID:".$output[0];
echo "OutPutFile:".substr($outputfile,6);

?>
