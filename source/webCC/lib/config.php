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
define('LOOP_COUNT',20);  //php parentにおいてループの際のusleepの秒数(milli sec)
define('READ_COUNT',1);   //phpBackGroundProcessにおいて初期表示のループをスキップする数
define('WRITE_WAIT',1000);   //param.phpにて書き込んだあとの待ち時間
define('MIN_F_SIZE' , 16 );
define('MAX_F_SIZE' , 28 );
define('DATA_SAVE_PASS' , '../tmp/' );
define('TEMP_FILE_PREFIX','nis');
define('TEMP_PDF_PREFIX','FRT');

//add env koyama 20150125
putenv('STN000=STN000');
putenv('JRCODE_PATH=../tmp/JRCODE_');
putenv('ERROR_PATH=../tmp/');
putenv('PIPE_PATH=../tmp/');
putenv('PATH="$PATH:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin"');
require_once('./lib/log.php');
require_once('./lib/constant.php');
?>