<?php
///
///設定ファイル
///(定義やdefineをまとめておく)
ini_set('default_charset', 'Shift_JIS');
define('SITE_TITLE','MAP');
define('SOURCEDIR','../');
define('DEFFNAME','menu/gmenu.sh');
define('DEFKIND','');
define('DB_NAME','testdb');
define('DB_PORT', 3306);
define('DB_HOST','127.0.0.1');
define('DB_USER','mysql');
define('DB_PASS','mysql');
define('LOGSETFILE' , false );
define('CHARSET' , 'SJIS-win' );
define('EXEC_LIVE',5760); //php parentにおいてループ開始から強制終了までの時間(分)
define('EXEC_SLEEP',200);  //php parentにおいてループの際のusleepの秒数(milli sec)
define('EXEC_MAX_END_WAIT', 3);	 // php runExecにおいてCOBOLの出力結果がgetOutに読み取られるまでプロセス終了を待機する最大秒数(秒)
define('WRITE_WAIT',1000);   //param.phpにて書き込んだあとの待ち時間
define('MIN_F_SIZE' , 16 );
define('MAX_F_SIZE' , 28 );
define('DATA_SAVE_PASS' , '../tmp/' );
define('TEMP_FILE_PREFIX','nis');
define('TEMP_PDF_PREFIX','FRT');
define('SSE_GETOUT_SLEEP', 1 * 1000 * 100); // php getOutにおいてSSEループの際のusleepの秒数(micro sec)
define('SSE_GETOUT_LIVE', 10); // php getOutにおいてSSEループ開始から切断までの時間(分) - 時間が長すぎると「104: Connection reset by peer」のエラーになりphp-fpmのプロセスが残る可能性がある。
define('INPUT_READ_WAIT', 100); // php clsAsynchronousProcessにおいてCOBOLへの入力パラメータがCOBOLに読み取られるまでの最大待機秒(milli sec)
define('INPUT_PROC_WAIT', 100); // php clsAsynchronousProcessにおいてCOBOLへの入力パラメータがCOBOLで処理されるまでの最大待機秒(milli sec)

//add env koyama 20150125
// RDBの読み替え
putenv('STRANYR-RDB=R-STRANYR');
// その他実行時環境変数
putenv('STN000=STN000');
putenv('JRCODE_PATH=../tmp/JRCODE_');
putenv('ERROR_PATH=../tmp/');
putenv('PIPE_PATH=../tmp/');
putenv('PATH="$PATH:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin"');
require_once('./lib/log.php');
require_once('./lib/constant.php');
?>
