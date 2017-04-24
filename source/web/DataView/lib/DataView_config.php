<?php
//変数定義
//画面上に表示するタイトル
define('SITE_TITLE','MAP');
//デバッグフラグ
define('DBUGFLG',false);
//エラーログ出力先
define('ERR_LOG_PATH','../../tmp/dataview.log');
//デバッグログ出力先
//define('NAKAMOTO_LOG_PATH','/home/n_test/public_html/source/web/DataView/nakamoto.log');
//CSV出力先
define('CSV_PATH','../../tmp/');
////////////////////////////////////////////////////////////////////////////////
//DataView用のDB設定
////////////////////////////////////////////////////////////////////////////////
define('DB_NAME','dataview');
if(DBUGFLG){
	//テストサーバー（中本ローカル）
	define('DB_HOST','192.168.1.105');
}else{
	//本番サーバー
	define('DB_HOST','127.0.0.1');
}
define('DB_PORT', 3306);
define('DB_USER','mysql');
define('DB_PASS','mysql');
////////////////////////////////////////////////////////////////////////////////
//nisshin用のDB設定
////////////////////////////////////////////////////////////////////////////////
define('NIS_DB_NAME','testdb');
define('NIS_DB_HOST','127.0.0.1');
define('NIS_DB_PORT', 3306);
define('NIS_DB_USER','mysql');
define('NIS_DB_PASS','mysql');
//何ページ先（前）までリンクを出すか
define('PAGINUM',3);
//テーブル選択時・候補追加ボタンで取得する項目の件数
define('GETITEMNUM',60);

//○○件毎にCSV化（1000バイトデータ×5000件＝5MのMYSQLメモリ使用）
define('CSVNUM',5000);


//表示しないテーブルを定義
$NOTDISPTABLE = array(
    "M_ITEMELEMENT"
    ,"M_ITEMELEMENT_bak"
    ,"B-TCM"
);
//add koyama 20150917 confとlibraryを分割
require_once('./lib/common.php');
?>
