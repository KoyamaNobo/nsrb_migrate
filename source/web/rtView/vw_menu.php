<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="index,follow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE ?> - <?php echo $result->title ?></title>
<script type="text/javascript" src="./js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<script type="text/javascript" src="./js/backButton.js"></script>
<script type="text/javascript" src="./js/menuShortcut.js"></script>
<script type="text/javascript" src="./js/userSetting.js"></script>
<?php
include('./rtView/vw_roexec_head.php');
?>
</head>
<body>
<div class="contentbody">
<div class="hblock">
	<div class="leftBlock">
		<h1><?php echo $result->title ?></h1>
	</div>
	<div class="rightBlock">
		<form name="foot" action="./index.php" method="get" class="foot">
			<div style="display:inline-block; width:50%;">
				<label for="root" >メニュー名：</label>
				<select id="root" name="root" >
					<option value="">&nbsp;</option>
<?php
foreach($_SESSION['pg_name'] as $strPgName){
	echo '				<option value="'.$strPgName.'">'.$strPgName.'</option>'.PHP_EOL;
}
?>
				</select>
			</div>
			<input class="button rootMenu" type="submit" name="send" value="選択" />
		</form>
	</div>
</div>
<div class="mblock">
	<div class="screen pmenu">
<?php
$tblLength = count($arrSelectItem);
$count = 0;
foreach($arrSelectItem as $item){
	$count++;
	if($count === 1){
		if($tblLength <= 10){
			echo "<div class=\"centerBlock\">".PHP_EOL;
		}else{
			echo "<div class=\"leftBlock\">".PHP_EOL;
		}
	}
	if(empty($item[2]) || (substr($item[2],0,1) === '*')|| (substr($item[2],0,1) === '-')){
		$strPrint = "<pre class=\"menItem\"><a>".$item[1]." " . $item[4];
	}else{
		//lm|jsのときは次の画面にタイトルを渡す
		//lm|jsのときは次の画面に前の画面名を渡す
		$strTitle = "";
		$strPrevious = "";
		$tag         = "a";
		if(preg_match("/js|lm|sm/",strtolower($item[3]))){
			if(isset($_GET['filename'])){
				$strPrevious = "&amp;previous=".$_GET['filename']."";
			}
			$tag         = "span";
			$strTitle    = "&amp;title=".urlencode($item[4])."";
		}
		$strPrint = "<pre class=\"menItem\"><".$tag." class=\"menuURL\" href=\"./index.php?filename=".strtolower($item[2])."&amp;typ=".strtolower($item[3]).$strPrevious.$strTitle."\">".$item[1]." " . $item[4];
	}
	if($tblLength <= 10){
		$strPrint .=" (".$item[2].")</a></pre>".PHP_EOL;
	}else{
		$strPrint .=" </a></pre>".PHP_EOL;
	}
	echo $strPrint;	
	if(($tblLength > 10 && $tblLength <= 20 && $count === 10) || ($tblLength > 20 && $count === 20)){
		echo "</div>".PHP_EOL;
		echo "<div class=\"rightBlock\">".PHP_EOL;
	}
}
if($tblLength > 0){
	echo "</div>".PHP_EOL;
}
?>
	</div>
</div>
<div class="status">
	<div id="status1"><span></span>
	</div>
	<div id="status2"><span></span>
	</div>
	<div id="status3"><span></span>
	</div>
	<div id="status4"><span><?php echo $jobName ?></span>
	</div>
	<div id="status5"><span></span>
	</div>
	<div id="status6"><span></span>
	</div>
	<div id="status7"><span></span>
	</div>
</div>
<div class="fblock">
	<div class="leftBlock">
		<label for="menuno" >メニュー番号：</label>
		<input class="menuno" type="text" id="menuno" name="menuno" maxlength="2" />
		<input class="uset" type="hidden" id="fsize" name="fsize" value="<?php if(isset($_SESSION['font_size']) && !empty($_SESSION['font_size'])){ echo $_SESSION['font_size']; } ?>" />
		<input class="uset" type="hidden" id="fcolor" name="fcolor" value="<?php if(isset($_SESSION['font_color']) && !empty($_SESSION['font_color'])){ echo $_SESSION['font_color']; } ?>" />
		<input class="uset" type="hidden" id="bgcolor" name="bgcolor" value="<?php if(isset($_SESSION['bg_color']) && !empty($_SESSION['bg_color'])){ echo $_SESSION['bg_color']; } ?>" />
		<input class="uset" type="hidden" id="sfcolor" name="sfcolor" value="<?php if(isset($_SESSION['reverse_font_color']) && !empty($_SESSION['reverse_font_color'])){ echo $_SESSION['reverse_font_color']; } ?>" />
		<input class="uset" type="hidden" id="sbgcolor" name="sbgcolor" value="<?php if(isset($_SESSION['reverse_bg_color']) && !empty($_SESSION['reverse_bg_color'])){ echo $_SESSION['reverse_bg_color']; } ?>" />
	</div>
	<div class="rightBlock">
		<input class="button backButton" type="button" value="戻る" />
	</div>
<?php
include('./rtView/vw_roexec_footer.php');
?>
</div>
</div>
</body>
</html>
