<?php

//共通ライブラリ読み込み
require_once('./lib/DataView_config.php');

////////////////////////////////////////////////////////////////////////////////
//実行時間無限大
////////////////////////////////////////////////////////////////////////////////
set_time_limit(0);

////////////////////////////////////////////////////////////////////////////////
//POSTの引数が存在しているか
////////////////////////////////////////////////////////////////////////////////
if(isset($_POST) && !empty($_POST) ){
	if(isset($_POST['Sql_str']) && !empty($_POST['Sql_str']) ){
		$post        = $_POST;
		$sql         = $post['Sql_str'];
		$outputfile  = CSV_PATH . $post['Output_file']; //レコード件数
	}else{
        echo('ERR0501');
        put_error('ERR0501','');
        exit();
	}
}

//SQL文からLIMIT文を外したものを取得（文末の「;」も外したもの）
$sqlExec = getSqlExec($sql);

//バックグラウンドでジョブ実行
$output     = array();
$sql = str_replace("'", "##", $sql);
$cmd = "php ./DataView_create_csv.php '".$sql."' >".$outputfile." & echo $! ";
exec($cmd,$output);

//戻り値としてプロセスIDとアウトプットファイル名を返す
echo "PID:".$output[0];

?>
