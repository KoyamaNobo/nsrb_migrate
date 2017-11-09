<?php
//		<form name="screenSub" action="./index.php" method="get" class="screen">
//
// print_r($screen->arrScreenStr);
if($termFlg == true){
	?>
			<iframe src="<?php echo (empty($_SERVER["HTTPS"]) ? "http://" : "https://") . $_SERVER['HTTP_HOST']; ?>/term/"  name="sample" width="750" height="500">この部分はインラインフレームを使用しています。</iframe> 
<?php
}else{
	foreach($screen->arrScreenStr as $key=>$view){
		echo $view->strStartTag.PHP_EOL;
		echo $view->echoElem.PHP_EOL;
		echo $view->strEndTag.PHP_EOL;
	}
}
//		</form>
?>