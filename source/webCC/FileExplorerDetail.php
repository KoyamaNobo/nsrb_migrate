<?php
session_start();
require_once('./lib/config.php');

//���ł�isset�Ŕ��肵�ĉ��
$oConf       = New initConf();                    //config.php���R���t�B�O�p�N���X
$nextExeName = $oConf->getFileExpDetail('detail');
$oConf->setstrExplorerPath(1);                    //fileExplorer

require_once('./DataExchangeDetail.php');
?>