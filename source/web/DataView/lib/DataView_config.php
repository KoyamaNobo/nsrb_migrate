<?php
//�ϐ���`
//��ʏ�ɕ\������^�C�g��
define('SITE_TITLE','MAP');
//�f�o�b�O�t���O
define('DBUGFLG',false);
//�G���[���O�o�͐�
define('ERR_LOG_PATH','../../tmp/dataview.log');
//�f�o�b�O���O�o�͐�
//define('NAKAMOTO_LOG_PATH','/home/n_test/public_html/source/web/DataView/nakamoto.log');
//CSV�o�͐�
define('CSV_PATH','../../tmp/');
////////////////////////////////////////////////////////////////////////////////
//DataView�p��DB�ݒ�
////////////////////////////////////////////////////////////////////////////////
define('DB_NAME','dataview');
if(DBUGFLG){
	//�e�X�g�T�[�o�[�i���{���[�J���j
	define('DB_HOST','192.168.1.105');
}else{
	//�{�ԃT�[�o�[
	define('DB_HOST','127.0.0.1');
}
define('DB_PORT', 3306);
define('DB_USER','mysql');
define('DB_PASS','mysql');
////////////////////////////////////////////////////////////////////////////////
//nisshin�p��DB�ݒ�
////////////////////////////////////////////////////////////////////////////////
define('NIS_DB_NAME','testdb');
define('NIS_DB_HOST','127.0.0.1');
define('NIS_DB_PORT', 3306);
define('NIS_DB_USER','mysql');
define('NIS_DB_PASS','mysql');
//���y�[�W��i�O�j�܂Ń����N���o����
define('PAGINUM',3);
//�e�[�u���I�����E���ǉ��{�^���Ŏ擾���鍀�ڂ̌���
define('GETITEMNUM',60);

//����������CSV���i1000�o�C�g�f�[�^�~5000����5M��MYSQL�������g�p�j
define('CSVNUM',5000);


//�\�����Ȃ��e�[�u�����`
$NOTDISPTABLE = array(
    "M_ITEMELEMENT"
    ,"M_ITEMELEMENT_bak"
    ,"B-TCM"
);
//add koyama 20150917 conf��library�𕪊�
require_once('./lib/common.php');
?>
