<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="noindex,nofollow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="stylesheet" href="./css/status_view.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE ?> - <?php echo $oStatus->title ?></title>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<?php
include('vw_status_js.php');
?>
</head>
<body>
<div class="contentbody">
<div class="hblock">
	<div class="leftBlock">
		<h1><?php echo $oStatus->title ?></h1>
	</div>
	<div class="rightBlock">
	</div>
</div>
<div class="mblock">
	<div class="screen pmenu">
		<?php echo $oStatus->getLineWithPageNum($page_num); ?>
	</div>
</div>
<div class="fblock">
	<div class="leftBlock">
		<input class="uset" type="hidden" id="fsize" name="fsize" value="<?php if(isset($_SESSION['font_size']) && !empty($_SESSION['font_size'])){ echo $_SESSION['font_size']; } ?>" />
		<input class="uset" type="hidden" id="fcolor" name="fcolor" value="<?php if(isset($_SESSION['font_color']) && !empty($_SESSION['font_color'])){ echo $_SESSION['font_color']; } ?>" />
		<input class="uset" type="hidden" id="bgcolor" name="bgcolor" value="<?php if(isset($_SESSION['bg_color']) && !empty($_SESSION['bg_color'])){ echo $_SESSION['bg_color']; } ?>" />
		<input class="uset" type="hidden" id="sfcolor" name="sfcolor" value="<?php if(isset($_SESSION['reverse_font_color']) && !empty($_SESSION['reverse_font_color'])){ echo $_SESSION['reverse_font_color']; } ?>" />
		<input class="uset" type="hidden" id="sbgcolor" name="sbgcolor" value="<?php if(isset($_SESSION['reverse_bg_color']) && !empty($_SESSION['reverse_bg_color'])){ echo $_SESSION['reverse_bg_color']; } ?>" />
		<div>
			<input id="listnum" class="" type="button" value="<?php if(property_exists($oStatus,'proc_array')){ echo (int)($oStatus->countStatusLine()).'行取得'; }else{ echo 'X行取得'; } ?>" />
			<input id="listmax" class="" type="hidden" value="<?php echo (int)$oStatus->countStatusMax(); ?>" />
		</div>
	</div>
	<div class="rightBlock">
		<input class="button page_change" type="button" value="<" />
		<label for="page_num" >ページ：</label>
		<input id="page_num" name="page_num" class="button page_num" type="button" value="<?php echo $page_num;?>" />
		<input class="button page_change" type="button" value=">" />
		<div style="display:inline-block;">
			<form name="foot" action="./status_view.php" method="get" class="foot">
				<label for="status_mode" >切替：</label>
				<input id="current_mode" name="current_mode" type="hidden" value="<?php echo $current_mode;?>" />
				<input id="status_mode" name="status_mode" type="hidden" value="<?php echo $status_mode;?>" />
				<input class="button rootMenu" type="submit" value="<?php echo $buttonName; ?>状態" />
			</form>
		</div>
	</div>
</div>
</div>
</body>
</html>
