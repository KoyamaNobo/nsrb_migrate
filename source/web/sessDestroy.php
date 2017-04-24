<?php
session_start();
session_destroy();
require_once('./lib/config.php');
$mess = '';
if(isset($_GET['mess'])){
	$mess = '?mess=' . urlencode($_GET['mess']);
}
if (isset($_COOKIE['PHPSESSID'])) {
    setcookie('PHPSESSID', '', time() - 1800, '/');
}
?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<meta http-equiv="content-type" content="text/html; charset=Shift_JIS"/>
<meta http-equiv="content-language" content="ja" />
<meta name="robots" content="index,follow" />
<meta name="rating" content="general" />
<meta http-equiv="refresh" content="0;URL=./index.php<?php echo $mess;?>">
<title><?php echo SITE_TITLE ?></title>
</head>
<body>
<p>エラーが発生しました。ログインページにジャンプします。</p>
<?php if(isset($_GET['mess']) && !empty($_GET['mess'])){ ?>
<p><?php echo $_GET['mess']; ?></p>
<?php } ?>
</body>
</html>
