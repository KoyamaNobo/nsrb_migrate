<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="noindex,nofollow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE; ?> - 処理中</title>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<script type="text/javascript" src="./js/backButton.js?ver=<?php echo filemtime('./js/backButton.js'); ?>"></script>
<script type="text/javascript" src="./js/userSetting.js?ver=<?php echo filemtime('./js/userSetting.js'); ?>"></script>
<script type="text/javascript" src="./js/dataUtilExec.js?ver=<?php echo filemtime('./js/dataUtilExec.js'); ?>"></script>
<script type="text/javascript" src="./js/specialKey.js?ver=<?php echo filemtime('./js/specialKey.js'); ?>"></script>
<script type="text/javascript" src="./js/inpCheckFromat.js?ver=<?php echo filemtime('./js/inpCheckFromat.js'); ?>"></script>
</head>
<body>
<div class="contentbody">
	<div class="hblock">
		<div class="leftBlock">
			<h1><?php echo $title; ?></h1>
		</div>
		<div class="rightBlock">
			<div style="display:inline-block; width:50%;">
				<label for="sk" >MENU：</label>
				<select id="skSelect" name="skSelect" >
					<option value="">&nbsp;</option>
					<option value="CS4">停止</option>
					<option value="CS5">再開</option>
					<option value="C8">プロ放棄</option>
					<option value="C9">業務放棄</option>
				</select>
			</div>
			<input class="button" id="skButton"  type="button" value="選択" />
		</div>
	</div>
	<div class="mblock">
		<div class="screen smart">
			<div class="line">
				<p><span style="font-weight:bold;font-size:180%;margin-top:10px;">データユーティリティ実行</span></p>
			</div>
			<div class="line">
				<p class="line" style="margin-left:70px;margin-top:50px;" ><span>下記データを貼り付けてください</span></p>
			</div>
			<div class="line" style="margin-top:10px;margin-left:70px;width:70%;height:40%;">
				<textarea class="line" style="display:inline-block;width:100%;height:100%;" id="execParam" readonly="readonly" ><?php echo $clsSmart->getParseText(); ?></textarea>
			</div>
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
