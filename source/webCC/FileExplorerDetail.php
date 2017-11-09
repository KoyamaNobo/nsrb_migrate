<?php
session_start();
require_once('./lib/config.php');

//下ではissetで判定して回避
$oConf       = New initConf();                    //config.php内コンフィグ用クラス
$nextExeName = $oConf->getFileExpDetail('detail');
$oConf->setstrExplorerPath(1);                    //fileExplorer

require_once('./DataExchangeDetail.php');
?>