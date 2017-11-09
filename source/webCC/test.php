<html>
<head>
<title>テスト</title>
</head>
<body>
<form action="./test.php" method="post">
<input type="text" name="name" />
<input id="live" type="checkbox" name="live" />
<label for="live" >生きている?</label>
<input id="sex_m" type="radio" name="sex" value="1" />男
<label for="sex_m" >男</label>
<input id="sex_f" type="radio" name="sex" value="2"/>女
<label for="sex_f" >女</label>
<input type="submit" name="" value="send" />
</form>
<pre>
<?php
if(isset($_GET)){
	print_r($_GET);
	echo __LINE__.__FILE__;
}
if(isset($_POST)){
	print_r($_POST);
}
?>
<?php
//ファイル取得
$tmp = array();
//4.0.0-RC2より前のバージョンでは、!== は存在しない
$cmdres = shell_exec('ls -alt');
$split_cmdres = explode(PHP_EOL,$cmdres);
foreach($split_cmdres as $line){
	$filestat =preg_split('/\s+/',$line);
	if(!is_null($filestat[8])){
		echo 'fname : '.$filestat[8].'</br>';
		echo 'size : '.$filestat[4].'</br>';
		echo $line.'</br>';
		echo '</br>';
	}
}
?>
</pre>
</body>
<html>