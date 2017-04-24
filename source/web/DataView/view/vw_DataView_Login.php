<?php
//header("Content-type: text/html; charset=SJIS");
//header("Cache-Control: Private");
?>
<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?ver=20160613"></script>
</head>
<body>
	<div class="contentbody">
		<div class="loginform">
				<h1>DataView ログイン</h1>
		</div>
		<div class="login">
			<div class="loginform">
				<form name="screenSub" action="./DataView_index.php" method="post" class="screen">
					<div class="dv_user">
						<label for="dv_user" class="inline"><pre>ユーザ名  ：</pre></label>
						<input type="text" id="dv_user_id" name="dv_user_id"  value="<?php if(!empty($post) && array_key_exists('dv_user_id',$post)){ echo $post['dv_user_id'] ; } ?>" maxlength="20" tabindex="1" />
					</div>
					<div class="dv_password">
						<label for="dv_password" class="inline"><pre>パスワード：</pre></label>
						<input type="password" id="dv_password" name="dv_password" value="" maxlength="20" tabindex="2" />
					</div>
					<input  class="loginButton" type="submit" name="loginsub" id="loginsub" value="ログイン" tabindex="3" />
				</form>
			</div>
			<pre id="errorMessage" class="errorMessage"><?php echo echoMessages($message_codes); ?></pre>
		</div>
		<div class="fblock">
			<div class="leftBlock">

			</div>
			<div class="rightBlock">
			</div>
		</div>
	</div>
</body>
</html>
