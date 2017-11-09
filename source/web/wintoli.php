<?php 
if (!preg_match('/192\.168\.*/',$_SERVER['REMOTE_ADDR'])) die(); 
?>
<?php 
	if(!array_key_exists(1,$argv)){
		echo '引数を一つ以上指定して下さい。';
	}
	$iii=0;
	$path_array = array();
	foreach($argv as $path_text){
		if($iii != 0){
			$path_array[] = $path_text;
		}
		$iii++;
	}
?>