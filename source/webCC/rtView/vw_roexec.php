<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="index,follow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE; ?> - コマンド処理中</title>
<script type="text/javascript" src="./js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<script type="text/javascript" src="./js/backButton.js"></script>
<script type="text/javascript" src="./js/userSetting.js"></script>
<?php
include('./rtView/vw_roexec_head.php');
?>
</head>
<body>
<div class="contentbody">
	<div class="hblock">
		<div class="leftBlock">
			<h1><?php if(array_key_exists('title',$sani)){ echo str_replace(' ','&nbsp;',$sani['title']);} ?></h1>
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
				<input type="hidden" name="typ" value="<?php if(array_key_exists('typ',$sani)){ echo $sani['typ']; } ?>" />
			</form>
		</div>
	</div>
	<div class="mblock">
		<div class="screen">
<?php
include('./rtView/vw_roexec_mid.php');
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
		</div>
<?php
//include('./rtView/vw_roexec_footer.php');
?>
	</div>
</div>
</body>
</html>