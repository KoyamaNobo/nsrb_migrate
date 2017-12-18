<?php
// FIXME �f�o�b�O�p�w�b�_�[�B��ō폜����B
header('Access-Control-Allow-Origin: *');

// �Z�b�V�����𗘗p����ƃ��N�G�X�g�ɔr�����b�N��������̂�SSE�ł̓Z�b�V�����͗��p���Ȃ��B
// session_start();
$sani = $_GET;
$pid = $sani['pid'];
$infname = $sani['infname'];
$outfname = $sani['outfname'];

if (! empty($pid) && ! empty($infname) && ! empty($outfname)) {
	require_once ('./lib/config.php');
	require_once ('./lib/log.php');
	require_once ('./lib/clsAsynchronousProcess.php');
	require_once ('./lib/clsScreen.php');

	$oLog = new Log('');
	$clsAP = new AsynchronousProcess($infname, $outfname);
	$screen = new clsScreen();

	$acceptHeader = $_SERVER['HTTP_ACCEPT'];

	// Accept�w�b�_�[�ɍ��킹�ď�����؂蕪����B
	if ($acceptHeader === 'text/event-stream') {
		// SSE
		header('Content-Type: text/event-stream;charset=UTF-8');
		header('Cache-Control: no-cache');

		$lastRes = '';

		while (1) {
			list ($res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);

			if (strcmp($res, $lastRes) === 0) {
				// �O��Ɠ������e�̏ꍇ�͉�������Ȃ��B(�ڑ����ؒf����Ȃ��悤�Ƀ|�[�����O�f�[�^��������)
				$lastRes = $res;

				echo ":\n\n";
			} else {
				$lastRes = $res;

				$res = "data: " . $res . "\n\n";
				$res = mb_convert_encoding($res, "UTF-8", "SJIS-WIN");

				$screen->screenParse($res);
			}

			ob_flush();
			flush();

			if ($is_break) {
				break;
			}

			// 1�b����(�}�C�N���b�w��)
			// FIXME �萔��
			usleep(1 * 1000 * 1000);
		}
	} else {
		// �ʏ�̃��N�G�X�g
		list ($res, $is_break) = get_response($oLog, $clsAP, $pid, $infname, $outfname);
		$screen->screenParse($res);
	}
}

function get_response($oLog, $clsAP, $pid, $infname, $outfname)
{
	$is_break = false;

	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	$res = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">';
	$res .= $clsAP->pRead();

	// �v���Z�X�I�����̖߂�Ή�
	// �w��̃v���Z�XID�����s�����`�F�b�N
	$state = Unix_IsPidExisted($pid, $infname, $outfname, $oLog);
	// $oLog->info('microtime(true) = '.microtime(true).__FILE__.__LINE__);
	if ($state === false) {
		// �I�����Ă���ꍇ�͈ȉ���input���o��
		$res .= '<input id="parentStatusGet" type="hidden" class="parentStatusGet" value="end">';
		// FIXME �v���Z�X���I����������������p������K�v�����邩�ǂ���������Ȃ��̂ň�U�������p������B
		$is_break = false;
	} else {
		$is_break = false;
	}

	// SSE�͘A��������s�R�[�h���I�[�����Ƃ��Ĉ�����̂ŁA�f�[�^���̉��s�R�[�h�͂��ׂč폜����B
	$res = preg_replace("/\r\n|\r|\n/", "", $res);

	return array(
		$res,
		$is_break
	);
}
?>
