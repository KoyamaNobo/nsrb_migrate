<?php
session_start();

//���ʃ��C�u�����ǂݍ���
require_once('./lib/DataView_config.php');

//�ϐ�������
$message_codes = "";


//�Z�b�V������j�󂷂�
//�N�b�L�[������������Έȉ��̗�̋L�q�ō폜���邱��
//  setcookie("�N�b�L�[�̕ϐ���", $count, time() - 1800);
//session_destroy();
//���̃Z�b�V�������󂳂Ȃ����߂�
$_SESSION['dv_user_id']            = '';
$_SESSION['dv_user_password']      = '';
$_SESSION['dv_user_authority_flg'] = '';
$_SESSION['dv_user_disp_num']      = '';
//��ʕ`��
require_once('./view/vw_DataView_Login.php');

?>
