/**
 * �t�H�[�J�X�ݒ���s���Ԋu(msec)
 */
var SET_FOCUS_INTERVAL = 100;
/**
 * Beep����炷�Ԋu(msec)
 */
var BEEP_INTERVAL = 600;
/**
 * �Z�b�V�����p���p�|�[�����O�Ԋu(msec)
 */
var SESSION_ALIVE_INTERVAL = 1000 * 60 * 5;

/**
 * ��������\���L���[�B �L���[�Ƀf�[�^�����݂���ꍇ�̓L�[���͂��u���b�N���ăo�b�t�@�����O����B
 * �������A���{����͂��u���b�N���邱�Ƃ͂ł��Ȃ��̂ŁA���[�}�����͂Ɠ��{����͂Ƃł� ���삪�قȂ邱�ƂɂȂ�B
 */
var processQueue = new Array();

/**
 * �L�[���͂��o�b�t�@�����O�������X�g
 */
var keyBuffer = new Array();

/**
 * history.back��1�x�ȏサ�Ȃ�����
 */
var historyBackFlg = false;

/**
 * ���ݔ������̃G���[
 */
var currentErrors = new Array();

/**
 * ��ʃf�[�^�̃^�C���X�^���v�B �Â��f�[�^���X�L�b�v���邽�߂̔���p
 */
var dataTimestamp = 0;

/**
 * STOP�����ۂ� �����L�[�����͂����܂œ��͂��󂯕t���Ȃ�
 */
var isStop = false;

/**
 * Beep�����w��񐔖炷���߂Ɏg���J�E���B �}�C�i�X�l�̏ꍇ��Beep���͒�~����\���B
 */
var beepCounter = -1;

/**
 * Beep����炷���߂̃^�C���A�E�g�֐���ID
 */
var beepTimeoutId;

/**
 * �u���E�U��Firefox���ۂ�
 */
var isFirefox = (function() {
	var userAgent = window.navigator.userAgent.toLowerCase();
	return userAgent.indexOf('firefox') != -1;

})();

/**
 * ��ʂ̏����������������鏈���̑O�������s���B
 */
var preProcess = function() {
	processQueue.push('');
}

/**
 * ��ʂ̏����������������鏈���̌㏈�����s���B
 */
var postProcess = function() {
	processQueue.pop();

	keyBufferSimulate();
}

/**
 * ���j���[��ʂ��ۂ�
 */
var isMenuScreen = function() {
	var infname = $('#infname');
	// menu���̏ꍇinfname���Ȃ�
	if (infname.length == 0) {
		return true;
	}

	return false;
}

/**
 * �O��ʂɖ߂�B
 */
var historyBack = function() {
	if (!historyBackFlg) {
		history.back();
		historyBackFlg = true;
	}
}

/**
 * �v���Z�X���I�����Ă��邩�ۂ�
 */
var isProcessEnd = function() {
	$parentStatus = $('#parentStatus');

	if (0 < $parentStatus.length) {
		if ($parentStatus.val() == 'end') {
			// �v���Z�X�I��
			return true;
		}
	}

	return false;
}

/**
 * �G���[���������Ă��邩�ۂ�
 */
var hasError = function() {
	let html = $('#status1').html();
	return 0 < $.trim(html.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g, '')).length;
}

/**
 * ��ʂ̗v�f�����ւ���B
 */
var screenReplace = function(resultTxt) {
	var $tarScreen = $('.screen');
	var $chgScreen = $('<div>');
	$chgScreen.html(resultTxt);

	if (!checkTimestamp($chgScreen)) {
		return;
	}

	if (!checkStatus($chgScreen)) {
		return;
	}

	// �擾�s�̂��ꂩ��
	replaceScreenElement($tarScreen, $chgScreen);
}

/**
 * ��ʃf�[�^�̃^�C���X�^���v���`�F�b�N����B
 */
var checkTimestamp = function($chgScreen) {
	$dataTimestamp = $chgScreen.find('#dataTimestamp');
	newDataTimestamp = parseFloat($dataTimestamp.val());

	// �ǂݎ���ɗv�f�͍폜
	$dataTimestamp.remove();

	if (dataTimestamp < newDataTimestamp) {
		// �f�[�^���V�����Ȃ��Ă���ꍇ
		dataTimestamp = newDataTimestamp;
		return true;
	}

	return false;
}

/**
 * �����̃X�e�[�^�X�ɍ��킹�ăX�e�[�^�X���X�V���������I������B
 */
var checkStatus = function($chgScreen) {
	// $chgScreen�̒�����error�T���ĕ\��
	$errors = $chgScreen.find('.error');
	if (0 < $errors.length) {
		// �G���[����
		var errorValue = $errors[0].value;
		var $status1 = $('#status1');
		$status1.html('<span>' + errorValue + '</span>');
	}

	// parentStatusGet�����݂��Ă���ꍇparentStatus�ɑ������
	// $chgScreen�̒�����parentStatusGet������
	let $parentStatusGet = $chgScreen.find('.parentStatusGet');
	if (0 < $parentStatusGet.length) {
		$('#parentStatus').val($parentStatusGet.val());
		$parentStatusGet.remove();
	}

	// �v���Z�X���I�������ꍇ�͉�ʍX�V��SSE���~
	if (isProcessEnd()) {
		stopScreenUpdateListener();
	}

	// �G���[���������Ă��Ȃ��ăv���Z�X���I�����Ă���ꍇ�͑O��ʂɖ߂�B(��ʂ��I������)
	if (isProcessEnd() && !hasError()) {
		historyBack();
		return false;
	}

	return true;
}

/**
 * ��ʓ��e���X�V����
 */
var replaceScreenElement = function($tarScreen, $chgScreen) {
	// line�N���X�����v�f�̎q��input�v�f�𒊏o
	let $tarInputs = $tarScreen.find('.line > input[type="text"]');
	let $chgInputs = $chgScreen.find('.line > input[type="text"]');

	// STOP����B��ʂ�����������O�ɔ��肵�Ă����B
	let isTarStop = false;
	let isChgStop = false;
	if (0 < $tarInputs.length) {
		let tarInput = $tarInputs[0];
		isTarStop = tarInput.name == 'STOP';
	}
	if (0 < $chgInputs.length) {
		let chgInput = $chgInputs[0];
		isChgStop = chgInput.name == 'STOP';
	}

	// �v�f�̓���ւ����@������
	// ���݂Ǝ��̉�ʂɓ������͍��ڂ�����ꍇ�̂ݍs�P�ʂŗv�f�̓���ւ����s���B
	let isLineChange = false;
	if (0 < $tarInputs.length && 0 < $chgInputs.length) {
		let tarInput = $tarInputs[0];
		let chgInput = $chgInputs[0];
		if (tarInput.name == chgInput.name) {
			var tarColumns = tarInput.className.match(/\s+f[0-9]+/i);
			var chgColumns = chgInput.className.match(/\s+f[0-9]+/i);

			if (tarColumns[0] == chgColumns[0]) {
				// name�������ł��N���X�̃t�B�[���h��������
				isLineChange = true;
			}
		}
	}

	// �_�ŕ\���̏�Ԃɍ��킹��
	setBlinkToElement($chgScreen);

	// �v�f�̓���ւ����s��
	if (isLineChange) {
		// �s�u������
		// ���݂̉�ʂ���r���v�f�̎q���폜
		$tarScreen.children(':not(.line)').remove();
		// ���l�Ɏ��̉�ʂ���r���v�f���폜�B�������A������͌�Ń}�[�W����̂ŗv�f�͎c���Ă����B
		let $chgNotLines = $chgScreen.children('div:not(.line)');
		$chgNotLines.remove();

		// ���݂̉�ʂ��x�[�X�ɍs�P�ʂ̓���ւ��������s��
		$tarLines = $tarScreen.children();
		for (i = 0; i < $tarLines.length; i++) {
			let tarLine = $tarLines[i];
			// ���̉�ʂ��瓯���N���X�����v�f�𒊏o
			// �N���X���̓X�y�[�X�Ōq����ĕ����擾�ł���̂ŁA�Z���N�^�ŃN���X����AND�����ɂȂ�悤��.�ɒu�����Ē��o
			let $chgLines = $chgScreen.children('.'
					+ tarLine.className.replace(' ', '.'));

			// �u�������Ώۂ̗v�f��������Ȃ��ꍇ�͉��������ɃX�L�b�v
			if ($chgLines.length == 0) {
				continue;
			}

			let $tarLine = $(tarLine);
			let $chgLine = $($chgLines[0]);

			// ���͗v�f���Ȃ��s�̏ꍇ�͒P���ɓ��e�����ւ�
			if ($tarLine.find('input[type="text"]').length == 0) {
				$tarLine.html($chgLine.html());
				$chgLine.remove();
				continue;
			}

			// ���͗v�f�̂���s�͓��͗v�f�ȊO���폜���ē���ւ����s���B
			$tarLine.children(':not(input[type="text"])').remove();
			$tarLine.append($chgLine.children(':not(input[type="text"])'));
			$chgLine.remove();
		}

		// �r���v�f���܂Ƃ߂Ēǉ�
		let addHtml = '';
		for (i = 0; i < $chgNotLines.length; i++) {
			addHtml += $chgNotLines[i].outerHTML;
		}
		$tarScreen.append(addHtml);
	} else {
		// �ۂ��ƒu������
		$tarScreen.html($chgScreen.html());
	}

	// ��ʂ����ւ������ʐF�č\��
	userSetting();

	// STOP����M�����Ƃ��̏���
	if (isTarStop && isChgStop) {
		// STOP�p��
	} else if (isTarStop) {
		// STOP��������
		stopStop();
	} else if (isChgStop) {
		// STOP�����ꂽ
		startStop();
	}

	// ���[�_��
	$errBuz = $chgScreen.find('#err-buz');
	if (0 < $errBuz.length) {
		startBeep(parseInt($errBuz.val()));
	}
	// ���̂�
	$infoBuz = $chgScreen.find('#info-buz');
	if (0 < $infoBuz.length) {
		startBeep(parseInt($infoBuz.val()));
	}

	// �t�H�[�J�X�����킹��
	setNextInputFocus();
}

/**
 * �_�ŕ\���ݒ���s�������B
 */
var setBlinkToElement = function($chgScreen) {
	targetArray = $chgScreen.find('.blink');
	var ii = 0;
	if (typeof (blinkVisibleFlg) == 'undefined' || blinkVisibleFlg == false) {
		for (ii = 0; ii < targetArray.length; ii++) {
			targetArray[ii].style.visibility = 'visible';
		}
	} else {
		for (ii = 0; ii < targetArray.length; ii++) {
			targetArray[ii].style.visibility = 'hidden';
		}
	}
	targetArray = null;
}

/**
 * ����I�ɉ�ʓ��e���X�V���鏈���B
 */
let esScreen;
var screenUpdate = function() {
	let setFocusTimeoutId;

	if (!isMenuScreen()) {
		// PID���擾�ł��Ȃ��ꍇ�͑O��ʂɖ߂菈���͉����s��Ȃ��B
		if ($('#pid').length == 0) {
			historyBack();
			return;
		}

		// ��������
		init();

		// SSE�J�n
		startScreenUpdateListener();

		// ���͗��Ƀt�H�[�J�X�����킹�鏈�����J�n
		setFocusTimeoutId = setTimeout(setInputFocus, SET_FOCUS_INTERVAL);

		/**
		 * ��ʑJ�ڑO��SSE�̐ڑ���ؒf����B
		 */
		$(window).on('beforeunload', function() {
			stopScreenUpdateListener();

			// �t�H�[�J�X�^�C�}�[����~
			clearTimeout(setFocusTimeoutId);
		});
	}

	/**
	 * �����������s���B (SSE�̃C�x���g���X�i�[���J�n�����܂ł�1�A2�b���x�̃^�C�����O������̂ŏ��߂�Ajax�Œl���擾����B)
	 */
	function init() {
		$.ajax({
			type : 'GET',
			url : 'getOut.php',
			data : getQueryParams(),
			beforeSend : function(jqXHR) {
				preProcess();
			},
			success : function(msg, txt) {
				onmessage(msg);
			},
			error : function(jqXHR, textStatus, errorThrown) {
				// �C�x���g���X�i�[�̂ق��ōď������s���̂ł����ł͉������Ȃ��B
				if (errorThrown) {
					console.log('BackGround connect Error' + textStatus + ':'
							+ errorThrown.message);
				}
			},
			complete : function() {
				postProcess();
				polling();
			}
		});
	}

	/**
	 * �Z�b�V�����^�C���A�E�g�΍�Ƃ��Ē���I�Ƀ��N�G�X�g���s���B
	 */
	function polling() {

		// �ʐM���ʂ͖�������
		setTimeout(function() {
			$.ajax({
				type : 'GET',
				url : 'getOut.php',
				data : getQueryParams(),
				complete : function() {
					polling();
				}
			});
		}, SESSION_ALIVE_INTERVAL);
	}

	/**
	 * ��ʓ��e���X�V����SSE���J�n����B
	 */
	function startScreenUpdateListener() {
		let url = buildUrl('getOut.php', getQueryParams());

		esScreen = new EventSource(url);
		esScreen.onmessage = function(e) {
			// ���b�Z�[�W��M
			preProcess();

			try {
				onmessage(e.data);
			} finally {
				postProcess();
			}
		};
	}

	/**
	 * �p�����[�^���擾����B
	 */
	function getQueryParams() {
		let queryParams = {
			pid : $('#pid').val(),
			infname : $('#infname').val(),
			outfname : $('#outfname').val(),
		};

		return queryParams;
	}

	/**
	 * ���͗��Ƀt�H�[�J�X�����킹��B
	 */
	function setInputFocus() {
		clearTimeout(setFocusTimeoutId);

		setNextInputFocus();

		setFocusTimeoutId = setTimeout(setInputFocus, SET_FOCUS_INTERVAL);
	}

	/**
	 * ��M�������b�Z�[�W����������B
	 */
	function onmessage(data) {
		// ��ʓ��e�̍X�V
		screenReplace(data);
	}

	/**
	 * URL��g�ݗ��Ă�
	 */
	function buildUrl(url, queryParams) {
		let param = $.param(queryParams);

		if (-1 < url.indexOf('?')) {
			return url + '&' + param;
		} else {
			return url + '?' + param;
		}
	}
};

/**
 * ���͗��Ƀt�H�[�J�X�����킹��B
 */
function setNextInputFocus() {
	if (document.activeElement.id != 'skSelect') {
		$('input.nextinput:last').focus();
	}
}

/**
 * ��ʓ��e���X�V����SSE���~����B
 */
function stopScreenUpdateListener() {
	if (esScreen) {
		esScreen.close();
		esScreen = null;
	}
}

/**
 * �o�b�t�@�����O�����L�[���͂����s����
 *
 */
function keyBufferSimulate() {
	// �G���[���b�Z�[�W���\���� ���v���Z�X�I�����̓L�[���얳���Ȃ��߃o�b�t�@�N���A
	if (isProcessEnd() && hasError()) {
		keyBufferClear();
		return;
	}

	// �������̏ꍇ�͉��������ɏI��
	if (0 < processQueue.length) {
		return;
	}

	// �o�b�t�@�ɉ����Ȃ��ꍇ�͉������Ȃ��B
	if (keyBuffer.length == 0) {
		return;
	}

	let $targets = $('input.nextinput');

	if (0 < $targets.length) {
		// �܂Ƃ߂đ������̂͑���
		if (sendBatchParam($targets[0])) {
			return;
		}
	}

	for (var i = 0; i < 1; i++) {
		e = keyBuffer.shift();

		// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
		if (screenSwitch(e)) {
			break;
		}

		// CTL+ANY�L�[�̏���
		if (funcSpecialKey(e)) {
			break;
		}

		if ($targets.length == 0) {
			// FIXME ��ʂ��؂�ւ��O�Ȃ̂ō��ڂ��Ȃ��ꍇ������B�Ȃ̂ŁA���͍��ڂ������܂ōĎ��s����K�v����B
			// �������͓��͗��֌W�Ȃ�POST���Ă����H���̏ꍇ�͑����`�F�b�N��MAXLENGTH�͖������邱�ƂɂȂ�B
			// FIXME ��U�ď���������
			keyBuffer.unshift(e);
			if (true)
				break;
			//

			console.log('no input');
			// ���͗������݂��Ȃ��ꍇ
			// F1�`F12�̃L�[ (FIXME ���̏����K�v�H)
			if (e.key.match(/F[0-9]{1,2}/)) {
				if (e.ctrlKey == true) {
					sendFunctionKey(e);
				}
			}
			break;
		}

		// F11�`F12�͎g�p�֎~
		if (e.key == 'F11' || e.key == 'F12') {
			break;
		}

		let target = $targets[0];

		let inputValue = '';
		let isInputCheck = true;

		// Backspace�AEscape�̏ꍇ�͓��͒l�͕s�v�Ȃ̂Ń`�F�b�N���Ȃ��B
		if (e.key != 'Backspace' && e.key != 'Escape') {
			// ���̓`�F�b�N
			if (elementInpCheck(target)) {
				// ���̓t�H�[�}�b�g�i�����Ή��j
				inputValue = elementInpFormat(target);
				isInputCheck = true;
			} else {
				isInputCheck = false;
			}
		}

		// �ʏ�̓��͕������ۂ�
		if (isInputCharValue(e)) {
			if (insertCharValue(e, target)) {
				break;
			}

			// �����ɒǉ�
			appendCharValue(e, target);

			// FIXME �w�i�F�ݒ� ��U���̃^�C�~���O�ŁB
			setInputColor();

			break;
		}

		// DELETE
		if (e.key == 'Delete') {
			delKey(target);
			break;
		}

		let sendParams = getSendParam(e, inputValue);

		// �T�[�o�[���M�s�v�ȃL�[�̏ꍇ�̓f�[�^���T�[�o�[�ɑ��M���Ȃ�
		if (sendParams[0] == false) {
			break;
		}

		// ���̓`�F�b�NNG�̏ꍇ�̓f�[�^���T�[�o�[�ɑ��M���Ȃ�
		if (!isInputCheck) {
			break;
		}

		ajaxSendParam({
			data : getKeyParams(sendParams[1], sendParams[2]),
			success : function(msg) {
				if (e.key == 'Enter') {
					// Beep�����~
					stopBeep();

					screenReplace(msg);
				} else if (e.key == 'Escape') {
					// Beep�����~
					stopBeep();

					if (isProcessEnd() && hasError()) {
						// �v���Z�X���I�����ăG���[���\������Ă���ꍇ�͑O��ʂ�
						historyBack();
					} else {
						screenReplace(msg);
					}
				} else {
					screenReplace(msg);
				}
			}
		});
	}

	// �ċA���s
	setTimeout(keyBufferSimulate, 100);
}

/**
 * �L�[�o�b�t�@�����O�������͓��e�̂����T�[�o�[�ɑ��M�ł���L�[���܂Ƃ߂đ��M����B
 */
function sendBatchParam(inputTarget) {
	let sendKeyParam = new Array();
	let isEnterKey = false;
	let isEscKey = false;
	// ���͒l����
	let isInput = 0 < inputTarget.value.length;

	// ���̓`�F�b�N���s�v�ȃL�[�̂݃p�����[�^�ɃZ�b�g���đ���
	for (var i = 0; i < keyBuffer.length; i++) {
		let e = keyBuffer[i];

		// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
		if (isScreenSwitch(e)) {
			break;
		}

		// CTL+ANY�L�[�̏���
		if (isFuncSpecialKey(e)) {
			break;
		}

		// F11�`F12�͎g�p�֎~�Ȃ̂ŃX�L�b�v
		if (e.key == 'F11' || e.key == 'F12') {
			keyBuffer.splice(i, 1);
			i--;
			continue;
		}

		// �ʏ�̓��͕������ۂ�
		if (isInputCharValue(e)) {
			break;
		}

		// DELETE
		if (e.key == 'Delete') {
			break;
		}

		let sendParams = getSendParam(e, '');

		// �����ȊO�̃T�[�o�[���M�s�v�ȃL�[�̏ꍇ�͉������Ȃ��Ƃ������ƂȂ̂ŃX�L�b�v����B
		if (sendParams[0] == false) {
			keyBuffer.splice(i, 1);
			i--;
			continue;
		}

		// ���̓`�F�b�N���s�v�ȃL�[�̂݃p�����[�^�ɃZ�b�g
		if (!isInput || e.key == 'Backspace' || e.key == 'Escape') {
			if (e.key == 'Escape') {
				isEscKey = true;
			} else if (e.key == 'Enter') {
				isEnterKey = true;
			}

			sendKeyParam.push({
				sendValue : sendParams[1],
				statusValue : sendParams[2]
			});

			keyBuffer.splice(i, 1);
			i--;
			continue;
		}

		break;
	}

	// ���M
	if (0 < sendKeyParam.length) {
		ajaxSendParam({
			data : getKeyBatchParams(sendKeyParam),
			success : function(msg) {
				if (isEscKey) {
					// Beep�����~
					stopBeep();

					if (isProcessEnd() && hasError()) {
						// �v���Z�X���I�����ăG���[���\������Ă���ꍇ�͑O��ʂ�
						historyBack();
					} else {
						screenReplace(msg);
					}
				} else if (isEnterKey) {
					// Beep�����~
					stopBeep();

					screenReplace(msg);
				} else {
					screenReplace(msg);
				}
			}
		});

		return true;
	}

	return false;
}

/**
 * �L�[�o�b�t�@�����O���N���A
 */
function keyBufferClear() {
	keyBuffer.length = 0;
}

/**
 * �L�[���͐���
 */
var keyControl = function() {
	// Firefox�̓��{����͊m�蔻��̂��߂̃t���O
	// Firefox�̏ꍇ�͓��{����͂̃L�[�C�x���g�͓��{��m�莞�̃G���^�[�L�[��keyup���ɂ݂̂ɔ�������B
	// ����������{����͂��ꂽ���ۂ��𔻒肷��B
	let isKeydown = false;

	$(document).keydown(
			function(e) {
				console.log('keydown:' + e.key + ':' + e.keyCode);

				// �G���[���b�Z�[�W���\���� ���v���Z�X�I�����̓L�[���얳��
				if (isProcessEnd() && hasError()) {
					return false;
				}

				if (isMenuScreen()) {
					// ���j���[���

					// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
					if (screenSwitch(e)) {
						return false;
					}

					// F1�`F12�AALT�AESC�͎g�p�֎~
					if (e.key.match(/F[0-9]{1,2}/) || e.key == 'Alt'
							|| e.key == 'Escape') {
						return false;
					}
				} else {
					// ���j���[�ȊO�̉��

					// ���{����͔���p
					if (isFirefox && e.key == 'Enter') {
						isKeydown = true;
					}

					if (0 < processQueue.length) {
						// ������
						// �L�[���͂��o�b�t�@�����O����B
						keyBuffer.push(e);
						console.log("buffer push:" + e.key + ":" + e.keyCode);

						return false;
					} else {
						if (0 < keyBuffer.length) {
							// �o�b�t�@�����O�����L�[���͂��c���Ă���ꍇ�͓��͂��ꂽ�L�[���o�b�t�@�̖����ɒǉ����A�L�[���s
							keyBuffer.push(e);
							keyBufferSimulate();

							return false;
						}

						// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
						if (screenSwitch(e)) {
							return false;
						}

						// CTL+ANY�L�[�̏���
						if (funcSpecialKey(e)) {
							return false;
						}

						let $targets = $('input.nextinput');

						if ($targets.length == 0) {
							// ���͗������݂��Ȃ��ꍇ
							// FIXME ���͍��ڂ����݂��Ȃ��ꍇ�̓o�b�t�@�ɒǉ����Ă�������I��
							// FIXME ���͍��ڂ��Ȃ���ʂ�����H
							keyBuffer.push(e);
							if (true)
								return false;
							//

							console.log('no input hon');
							// F1�`F12�̃L�[ (FIXME ���̏����K�v�H)
							if (e.key.match(/F[0-9]{1,2}/)) {
								if (e.ctrlKey == true) {
									sendFunctionKey(e);
								}
							}
							return false;
						}

						// F11�`F12�͎g�p�֎~
						if (e.key == 'F11' || e.key == 'F12') {
							return false;
						}

						let target = $targets[0];

						let inputValue = '';
						let isInputCheck = true;

						// Backspace�̏ꍇ�͓��͒l�͕s�v�Ȃ̂Ń`�F�b�N���Ȃ��B
						if (e.key != 'Backspace' && e.key != 'Escape') {
							// ���̓`�F�b�N
							// FIXME
							// ���̓`�F�b�N�̃^�C�~���O�����������B���ꂾ�ƃG���[���\�������^�C�~���O�����������Ȃ�B
							if (elementInpCheck(target)) {
								// ���̓t�H�[�}�b�g�i�����Ή��j
								inputValue = elementInpFormat(target);
								isInputCheck = true;
							} else {
								isInputCheck = false;
							}
						}

						// �ʏ�̓��͕������ۂ�
						if (isInputCharValue(e)) {
							if (insertCharValue(e, target)) {
								return false;
							}

							return true;
						}

						// DELETE
						if (e.key == 'Delete') {
							delKey(target);
							return false;
						}

						let sendParams = getSendParam(e, inputValue);

						// �T�[�o�[���M�s�v�ȃL�[�̏ꍇ�͓��̓`�F�b�N�������ĕ����̓��͂͋���
						if (sendParams[0] == false) {
							return true;
						}

						// ���̓`�F�b�NNG�̏ꍇ�̓f�[�^���T�[�o�[�ɑ��M���Ȃ�
						if (!isInputCheck) {
							return false;
						}

						ajaxSendParam({
							data : getKeyParams(sendParams[1], sendParams[2]),
							success : function(msg) {
								if (e.key == 'Enter') {
									// Beep�����~
									stopBeep();

									screenReplace(msg);
								} else if (e.key == 'Escape') {
									// Beep�����~
									stopBeep();

									if (isProcessEnd() && hasError()) {
										// �v���Z�X���I�����ăG���[���\������Ă���ꍇ�͑O��ʂ�
										historyBack();
									} else {
										screenReplace(msg);
									}
								} else {
									screenReplace(msg);
								}
							}
						});

						return false;
					}
				}

				return true;
			});

	if (!isMenuScreen()) {

		/**
		 * �L�[�A�b�v�C�x���g
		 */
		$(document).keyup(function(e) {
			// console.log('keyup:' + e.key + ':' + e.keyCode);
			let isProcess = false;

			if (isFirefox && e.key == 'Enter') {
				if (!isKeydown) {
					// ���{����͊m��̃G���^�[
					isProcess = true;
				}
				isKeydown = false;
			}

			// ���{����͂����ꂽ�ꍇ�͓��͏������t�]���Ă��܂��̂Ńo�b�t�@�̓N���A
			if (isProcess) {
				keyBufferClear();
			}

			// �w�i�F�ݒ�
			setInputColor();

			return true;
		});
	}
};

/**
 * ���͕������ۂ����肷��
 */
var isInputCharValue = function(e) {
	// CTL or ALT��������Ă���
	if (e.ctrlKey == true || e.altKey == true) {
		return false;
	}

	if (1 < e.key.length) {
		// �L�[��2�����ȏ�ŕ\�����ꍇ
		return false;
	}

	let code = e.key.charCodeAt(0);
	// ASCII�R�[�h�̃X�y�[�X�`~�܂ł͈̔�(����R�[�h����������)
	if (32 <= code && code <= 126) {
		return true;
	}

	return false;
}

/**
 * ������}������B �J�[�\���ʒu�������̏ꍇ�͐���s�v�Ȃ̂ŉ������Ȃ��B(false��ԋp)
 */
var insertCharValue = function(e, field) {
	let startPos = field.selectionStart;
	let fieldValue = field.value;

	// �����ɃJ�[�\��������ꍇ�͒ǋL�ɂȂ�̂ŉ������Ȃ�
	if (fieldValue.length <= startPos) {
		return false;
	}

	// maxLength�𒴂��镔���͐؂���
	let insertValue = e.key;
	let newValue = fieldValue.slice(0, startPos) + insertValue
			+ fieldValue.slice(startPos + insertValue.length);
	if (field.maxLength < newValue.length) {
		newValue = newValue.slice(0, field.maxLength);
	}

	// �V����������ݒ肵�ăJ�[�\���ʒu���Đݒ肷��
	field.value = newValue;
	field.selectionStart = startPos + insertValue.length;
	field.selectionEnd = startPos + insertValue.length;

	return true;
}

/**
 * �����𖖔��ɒǉ�����B
 */
var appendCharValue = function(e, field) {
	let fieldValue = field.value;

	// maxLength�𒴂���ꍇ�͉������Ȃ��B
	if (field.maxLength <= fieldValue.length) {
		return false;
	}
	let newValue = fieldValue + e.key;

	// �V����������ݒ肵�ăJ�[�\���ʒu���Đݒ肷��
	field.value = newValue;
	field.selectionStart = newValue.length;
	field.selectionEnd = newValue.length;

	return true;
}

/**
 * DELETE�L�[
 */
var delKey = function(field) {
	let startPos = field.selectionStart;
	let fieldValue = field.value;

	if (0 < startPos) {
		let newValue = fieldValue.slice(0, startPos - 1)
				+ fieldValue.slice(startPos);
		field.value = newValue;
		field.selectionStart = startPos - 1;
		field.selectionEnd = startPos - 1;
	}
}

/**
 * �T�[�o�[�ɑ��M����p�����[�^���擾����B
 */
var getSendParam = function(e, inputValue) {

	let statusValue = '';

	// ���͒l�̐ݒ�
	switch (e.key) {
	case 'Enter':
		if (e.shiftKey == true) {
			statusValue = 'f';
			break;
		}
		statusValue = 'h';
		break;
	case 'Escape':
		inputValue = '';
		statusValue = 'esc';
		break;
	case 'Tab':
		statusValue = 's';
		break;
	case 'Backspace':
		inputValue = '';
		statusValue = 'b';
		break;
	case 'F10':
	case 'Alt':
		if (e.shiftKey == true) {
			statusValue = '10';
		} else {
			statusValue = 'a';
		}
		break;
	case 'F1':
	case 'F2':
	case 'F3':
	case 'F4':
	case 'F5':
	case 'F6':
	case 'F7':
	case 'F8':
	case 'F9':
		if (e.key == 'F4' && e.shiftKey == true) {
			statusValue = '14';
			break;
		}
		if (e.key == 'F8' && e.shiftKey == true) {
			statusValue = 'c1';
			break;
		}
		if (e.key == 'F9' && e.shiftKey == true) {
			statusValue = 'c2';
			break;
		}
		statusValue = e.key.substr(1);
		break;
	case 'Process':
		// 2�o�C�g����
		// Firefox�ł͔������Ȃ��B
		return [ false, '', '' ];
	default:
		return [ false, '', '' ];
	}

	return [ true, inputValue, statusValue ];
}

/**
 * �w�i�F��ݒ肷��
 */
var setInputColor = function() {
	let $input = $(document.activeElement);

	if (!$input.hasClass('nextinput')) {
		// �A�N�e�B�u�ȍ��ڂ����͗��łȂ��ꍇ�̓Z���N�^�œ��͗����擾
		// �قƂ�ǂ̏ꍇ��activeElement�����͗��̂͂��Ȃ̂ŁA�Z���N�^�̃R�X�g���l�����ĈႤ�ꍇ�̂ݎ擾
		$input = $('input.nextinput');
	}

	if (0 < $input.length) {
		if (0 < $input.val().length) {
			// ���͂���
			$input.css('background-color', $('#sbgcolor').val());
		} else {
			$input.css('background-color', '');
		}
	}
}

/**
 * F1�`F12�̃R�[�h�𑗐M����B
 */
var sendFunctionKey = function(e) {
	ajaxSendParam({
		data : getKeyParams('F' + (e.keyCode - 111), ''),
		success : function(msg) {
			screenReplace(msg);
		}
	});
}

/**
 * �L�[���͂̃p�����[�^���擾����B
 */
var getKeyParams = function(value, statusValue) {
	let sendValue = value;

	if (0 < statusValue.length) {
		sendValue += '`' + statusValue;
	}

	let keyParams = {
		pid : $('#pid').val(),
		infname : $('#infname').val(),
		outfname : $('#outfname').val(),
		value : sendValue,
	};

	return keyParams;
}

/**
 * �L�[���͂̃p�����[�^���擾����B(�o�b�`�p)
 */
var getKeyBatchParams = function(sendKeyParam) {
	let valueParams = new Array();

	for (let i = 0; i < sendKeyParam.length; i++) {
		let sendValue = sendKeyParam[i].sendValue;
		let statusValue = sendKeyParam[i].statusValue;

		if (0 < statusValue.length) {
			sendValue += '`' + statusValue;
		}
		valueParams.push(sendValue);
	}

	let keyParams = {
		pid : $('#pid').val(),
		infname : $('#infname').val(),
		outfname : $('#outfname').val(),
		value : valueParams,
	};

	return keyParams;
}

/**
 * STOP�J�n
 */
var startStop = function() {
	isStop = true;
}

/**
 * STOP��~
 */
var stopStop = function() {
	isStop = false;
}

/**
 * STOP�����ۂ�
 */
var isStoping = function() {
	return isStop;
}

/**
 * Beep���̍Đ����J�n����
 */
var startBeep = function(soundLength) {
	// ���ɍĐ����Ă���ꍇ�͏㏑���Đ��͂��Ȃ�
	if (!isBeeping()) {
		beepCounter = 0;
		beep(soundLength);
	}
}

/**
 * Beep�����~�߂�
 */
var stopBeep = function() {
	if (isBeeping()) {
		clearTimeout(beepTimeoutId);
		beepCounter = -1;
	}
}

/**
 * Beep�����Đ������ۂ�
 */
var isBeeping = function() {
	return -1 < beepCounter;
}

/**
 * Beep�����Đ�����
 */
var beep = function(soundLength) {
	clearTimeout(beepTimeoutId);

	if (beepCounter++ < soundLength) {
		// base64�������\��t��(���̒����������ŃR���g���[������K�v�L��H
		var base64 = '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'
		// datauri scheme �`���ɂ��� Audio �I�u�W�F�N�g�𐶐����܂�
		var sound = new Audio('data:audio/wav;base64,' + base64);
		// ����炵�܂�
		sound.play();
		/*
		 * var msg = '�G���[���������܂����B\n'; msg += '�Ǘ��҂ɘA�����Ă��������B\n'; msg +=
		 * soundLength + ''; alert(msg); BuzzerFlg = 1;
		 */

		// ���̃^�C�}�[���Z�b�g
		beepTimeoutId = setTimeout(function() {
			beep(soundLength);
		}, BEEP_INTERVAL);
	}
}

// var Beep = function() {
// let beepCounter = -1;
// let beepTimeoutId = -1;
//
// function beep(soundLength) {
// if (-1 < beepTimeoutId) {
// clearTimeout(beepTimeoutId);
// }
//
// if (beepCounter++ < soundLength) {
// // base64�������\��t��(���̒����������ŃR���g���[������K�v�L��H
// var base64 =
// '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'
// // datauri scheme �`���ɂ��� Audio �I�u�W�F�N�g�𐶐����܂�
// var sound = new Audio('data:audio/wav;base64,' + base64);
// // ����炵�܂�
// sound.play();
// /*
// * var msg = '�G���[���������܂����B\n'; msg += '�Ǘ��҂ɘA�����Ă��������B\n'; msg +=
// * soundLength + ''; alert(msg); BuzzerFlg = 1;
// */
//
// // ���̃^�C�}�[���Z�b�g
// beepTimeoutId = setTimeout(function() {
// beep(soundLength);
// }, BEEP_INTERVAL);
// }
// };
//
// function isBeeping() {
// return -1 < beepCounter;
// }
//
// return {
// stopBeep : function() {
// if (isBeeping()) {
// clearTimeout(beepTimeoutId);
// beepCounter = -1;
// }
// },
// startBeep : function(soundLength) {
// // ���ɍĐ����Ă���ꍇ�͏㏑���Đ��͂��Ȃ�
// if (!isBeeping()) {
// beepCounter = 0;
// beep(soundLength);
// }
// },
// isBeeping : isBeeping(),
//
// }
// }
//
// var bee = new Beep();

/**
 * �p�����[�^���M�p��Ajax�g��
 */
var ajaxSendParam = function(arg) {
	var opt = $.extend({}, $.ajaxSettings, arg);

	opt.type = 'POST';
	opt.url = 'param.php';

	// �O����
	opt.beforeSend = (function(func) {
		return function(jqXHR) {
			// �O����
			preProcess();

			if (func) {
				func(jqXHR);
			}
		};
	})(opt.beforeSend);

	// ��������
	opt.success = (function(func) {
		return function(data, statusText, jqXHR) {
			if (func) {
				func(data, statusText, jqXHR);
			}
		};
	})(opt.success);

	// �G���[
	opt.error = (function(func) {
		return function(jqXHR, statusText, errorThrown) {
			alert('BackGround connect Error' + textStatus + ':'
					+ errorThrown.message);
			console.log('BackGround connect Error' + textStatus + ':'
					+ errorThrown.message);

			if (func) {
				func(jqXHR, statusText, errorThrown);
			}
		};
	})(opt.error);

	// ����
	opt.complete = (function(func) {
		return function(jqXHR, statusText) {
			if (func) {
				func(jqXHR, statusText);
			}

			// �㏈��
			postProcess();
		};
	})(opt.complete);

	return $.ajax(opt);
};

addEvent('load', window, screenUpdate);
addEvent('load', window, keyControl);
