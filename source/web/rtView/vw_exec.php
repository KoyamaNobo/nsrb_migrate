<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="index,follow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE; ?> - 処理中</title>
<script type="text/javascript" src="./js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<?php
include('./view/vw_exec_js.php');
?>
</head>
<body>
<div class="contentbody">
	<div class="hblock">
		<div class="leftBlock">
			<h1><?php echo str_replace(' ','&nbsp;',$sani['title']); ?></h1>
		</div>
		<div class="rightBlock">
			<form name="foot" action="./index.php" method="get" class="foot">
				<div style="display:inline-block; width:50%;">
					<label for="root" >メニュー名：</label>
					<select id="root" name="root" >
						<option value="">&nbsp;</option>
<?php
foreach($_SESSION['pg_name'] as $strPgName){
?>
				<option value="<?php echo $strPgName; ?>"><?php echo $strPgName; ?></option>
<?php
}
?>
					</select>
				</div>
				<input class="button rootMenu" type="submit" value="選択" />
				<input type="hidden" name="typ" value="<?php echo $sani['typ'] ?>" />
			</form>
		</div>
	</div>
	<div class="mblock">
		<div class="screen">
<?php
//		<form name="screenSub" action="./index.php" method="get" class="screen">
//
// print_r($screen->arrScreenStr);
foreach($screen->arrScreenStr as $key=>$view){
	echo $view->strStartTag.PHP_EOL;
	//echo表示がなければ中身の表示を試す
	if(empty($view->echoElem)){
		foreach($view->arrLineElem as $key=>$elem){
			//文字列の表示
			if($elem->type == 'TEX'){
				echo $elem->getText().PHP_EOL;
			}
			//入力項目の表示
			if($elem->type == 'INP'){
				echo $elem->getText().PHP_EOL;
			}
		}
	}else{
		echo $view->echoElem.PHP_EOL;
	}
	echo $view->strEndTag.PHP_EOL;
	foreach($view->arrLineElem as $key=>$elem){
		//罫線(下線)
		if($elem->type == 'UND'){
			echo $elem->getText().PHP_EOL;
		}
		//罫線(箱)
		if($elem->type == 'BOX'){
			echo $elem->getText().PHP_EOL;
		}
		//罫線(縦線)
		if($elem->type == 'VER'){
			echo $elem->getText().PHP_EOL;
		}
		//罫線(上線)
		if($elem->type == 'OVE'){
			echo $elem->getText().PHP_EOL;
		}
	}
}
//		</form>
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
		<div id="status4"><span></span>
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
<?php if(!empty($clsBP->tempnameOut)){ ?>
			<input id="outfname" type="hidden" name="outfname" value="<?php echo $clsBP->tempnameOut;?>" />
<?php } ?>
<?php if(!empty($clsBP->tempnameIn)){ ?>
			<input id="infname" type="hidden" name="infname" value="<?php echo $clsBP->tempnameIn;?>" />
<?php } ?>
<?php if(!empty($clsBP->getline_index)){ ?>
			<input id="line_index" type="hidden" name="line_index" value="<?php echo $clsBP->getline_index;?>" />
<?php } ?>
<?php if(!empty($clsBP->pid)){ ?>
			<input id="pid" type="hidden" name="pid" value="<?php echo $clsBP->pid;?>" />
			<input id="parentStatus" type="hidden" class="parentStatus" value="running">
<?php } ?>
			<input class="uset" type="hidden" id="fsize" name="fsize" value="<?php if(isset($_SESSION['font_size']) && !empty($_SESSION['font_size'])){ echo $_SESSION['font_size']; } ?>" />
			<input class="uset" type="hidden" id="fcolor" name="fcolor" value="<?php if(isset($_SESSION['font_color']) && !empty($_SESSION['font_color'])){ echo $_SESSION['font_color']; } ?>" />
			<input class="uset" type="hidden" id="bgcolor" name="bgcolor" value="<?php if(isset($_SESSION['bg_color']) && !empty($_SESSION['bg_color'])){ echo $_SESSION['bg_color']; } ?>" />
			<input class="uset" type="hidden" id="sfcolor" name="sfcolor" value="<?php if(isset($_SESSION['reverse_font_color']) && !empty($_SESSION['reverse_font_color'])){ echo $_SESSION['reverse_font_color']; } ?>" />
			<input class="uset" type="hidden" id="sbgcolor" name="sbgcolor" value="<?php if(isset($_SESSION['reverse_bg_color']) && !empty($_SESSION['reverse_bg_color'])){ echo $_SESSION['reverse_bg_color']; } ?>" />
		</div>
		<div class="rightBlock">
			<input  class="button backButton" type="button" value="戻る" />
			<div style="display:inline-block;">
				<form name="foot" action="./index.php" method="get" class="foot">
					<input type="hidden" name="typ" value="<?php echo $sani['typ'] ?>" />
				</form>
			</div>
		</div>
	</div>
</div>
</body>
</html>