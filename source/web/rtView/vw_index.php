<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta name="robots" content="index,follow" />
<meta name="rating" content="general" />
<link rel="stylesheet" href="./css/styleReset.css" type="text/css" />
<link rel="stylesheet" href="./css/index.css" type="text/css" />
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title><?php echo SITE_TITLE ?></title>
<script type="text/javascript" src="./js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
</head>
<body>
<div class="contentbody">
	<div class="hblock">
		<div class="leftBlock">
			<h1>ログイン</h1>
		</div>
		<div class="rightBlock">
		</div>
	</div>
	<div class="login">
		<div class="loginform">
			<form name="screenSub" action="./index.php" method="post" class="screen">
				<div class="inline account">
					<label for="account"><pre>ユーザ名  ：</pre></label><input type="text" id="account" name="account" maxlength="20" tabindex="1" />
				</div>
				<div class="inline passwd">
					<label for="passwd"><pre>パスワード：</pre></label><input type="password" id="passwd" name="passwd" maxlength="20" tabindex="2" />
				</div>
				<input  class="button" type="submit" name="loginsub" id="loginsub" value="ログイン" tabindex="3" />
			</form>
		</div>
<?php if(isset($message) && !empty($message)){ ?>
		<pre class="errorMess"><?php echo $message; ?></pre>
<?php } ?>
	</div>
	<div class="fblock">
		<div class="leftBlock">
			<div><a href="./DataView/DataView_index.php?" target="_blank"><input  class="button" type="button" value="DataView" /></a></div>
		</div>
		<div class="rightBlock">
			<input  class="button" type="button" value="戻る" disabled="disabled" />
		</div>
	</div>
</div>
</body>
</html>