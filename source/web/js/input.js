/**
 * �u�U�[����炷�Ԋu(msec)
 */
const BUZZER_INTERVAL = 600;
/**
 * �Z�b�V�����p���p�|�[�����O�Ԋu(msec)
 */
const SESSION_ALIVE_INTERVAL = 1000 * 60 * 5;
/**
 * �L�[���͂��Č�����ۂ̍Č��Ԋu(msec)
 */
const KEY_BUFFER_SIMULATE_INTERVAL = 10;
/**
 * ���͗��Ȃ��̉�ʂƔ��肷�鎞��(msec) �B�����Őݒ肵�����ԁA��ʂ�nextinput�̓��͗����\������Ȃ��ꍇ�͓��͗��Ȃ��̉�ʂƂ��Ĕ��f����B
 */
const NONE_INPUT_AREA_ELAPSED_TIME = 3000;

/**
 * �u���E�U��Firefox���ۂ�
 */
var isFirefox = (function() {
	var userAgent = window.navigator.userAgent.toLowerCase();
	return userAgent.indexOf('firefox') != -1;

})();

/**
 * ��������\���L���[�B �L���[�Ƀf�[�^�����݂���ꍇ�̓L�[���͂��u���b�N���ăo�b�t�@�����O����B
 * �������A���{����͂��u���b�N���邱�Ƃ͂ł��Ȃ��̂ŁA���[�}�����͂Ɠ��{����͂Ƃł� ���삪�قȂ邱�ƂɂȂ�B
 */
var processQueue = new Array();

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
	inputKey.keyBufferSimulate();
}

/**
 * ��ʕ\��������s���B
 */
var ScreenControl = function(buzzer) {

	/**
	 * STOP��Ԃ�\���t���O
	 */
	let _isInputStop = false;

	/**
	 * ��ʃf�[�^�̃^�C���X�^���v�B �Â��f�[�^���X�L�b�v���邽�߂̔���p
	 */
	let _dataTimestamp = 0;

	/**
	 * ��ʂ�SSE�ōX�V���邽�߂̃C�x���g�\�[�X
	 */
	let _esScreen;

	/**
	 * ��������public���\�b�h�ɃA�N�Z�X���邽�߂̕ϐ�
	 */
	let _that = this;

	/**
	 * �Ō�ɓ��͗������݂����Ƃ��̎��� (���͗������݂����ʂ��ۂ��𔻒f���邽�߂Ɏg�p����)
	 */
	let _lastHasInputTime;

	/**
	 * SSE�ł̐ڑ����m���������ۂ�
	 */
	let _isSSEOpen = false;

	/**
	 * �u�U�[����
	 */
	let _buzzer = buzzer;

	/**
	 * ���j���[��ʂ��ۂ����肷��B
	 */
	this.isMenuScreen = function() {
		let infname = $('#infname');
		// menu���̏ꍇinfname���Ȃ�
		if (infname.length == 0) {
			return true;
		}

		return false;
	}

	/**
	 * �O��ʂɖ߂�B
	 */
	this.historyBack = (function() {
		let historyBackFlg = false;

		return function() {
			if (!historyBackFlg) {
				history.back();
				historyBackFlg = true;
			}
		}
	})();

	/**
	 * �T�[�o�[�̃v���Z�X���I�����Ă��邩�ۂ����肷��B
	 */
	this.isProcessEnd = function() {
		let $parentStatus = $('#parentStatus');

		if (0 < $parentStatus.length) {
			if ($parentStatus.val() == 'end') {
				// �v���Z�X�I��
				return true;
			}
		}

		return false;
	}

	/**
	 * �G���[���e��\�����Ă��邩�ۂ����肷��B
	 */
	this.hasError = function() {
		let html = $('#status1').html();
		return 0 < $.trim(html.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g, '')).length;
	}

	/**
	 * �Ō�ɓ��͍��ڂ����݂������Ԃ��擾����B
	 */
	this.getLastHasInputTime = function() {
		return _lastHasInputTime;
	}

	/**
	 * �Ō�ɓ��͍��ڂ����݂������Ԃ�ݒ肷��B
	 */
	this.setLastHasInputTime = function(lastTime) {
		_lastHasInputTime = lastTime;
	}

	/**
	 * �����\���������s���B
	 */
	this.init = function() {
		// ���j���[�n�̉�ʂ̏ꍇ�͉������������Ȃ�
		if (_that.isMenuScreen()) {
			return;
		}

		// PID���擾�ł��Ȃ��ꍇ�͑O��ʂɖ߂艽�����������Ȃ�
		if ($('#pid').length == 0) {
			historyBack();
			return;
		}

		// Ajax�ŉ�ʃf�[�^�̎擾���s��
		screenUpdatePolling();

		// ��ʃf�[�^�擾�p��SSE���J�n����
		startScreenUpdateListener();

		// ���͗��Ƀt�H�[�J�X�����킹��
		setInputFocus();
		$(document).on('focusout', function(e) {
			// ���͗�����t�H�[�J�X���O��Ȃ��悤�Ƀt�H�[�J�X���O�ꂽ�ۂ̍Đݒ菈�����s���B
			// focusout�̒��Ńt�H�[�J�X�̍Đݒ肪�ł��Ȃ�����setTimeout�Ń^�C�~���O�����炷�B
			setTimeout(function() {
				setInputFocus();
			}, 1);
		});

		// ��ʑJ�ڑO��SSE�̐ڑ���ؒf����
		// �����I�ɐؒf���Ȃ��Ă����Ȃ���Firefox���Ƌ����ؒf���ꂽ�|�̃��b�Z�[�W���R���\�[���ɕ\������邽��
		$(window).on('beforeunload', function() {
			stopScreenUpdateListener();
		});
	};

	/**
	 * Ajax�ŉ�ʃf�[�^�̎擾���s���B(�|�[�����O����)
	 */
	let screenUpdatePolling = function() {
		// SSE�̐ڑ����m������܂ł͉�ʏ��������ړI�ŁB�ڑ��m����̓Z�b�V�����^�C���A�E�g���N�����Ȃ��ړI�Ń|�[�����O�Ԋu��������B
		// SSE�͐ڑ��܂łɃ^�C�����O������̂ŁA���߂�Ajax�ŉ�ʃf�[�^���擾����B
		let timeout = _isSSEOpen ? SESSION_ALIVE_INTERVAL : 200;

		setTimeout(function() {
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
				complete : function() {
					postProcess();
					screenUpdatePolling();
				},
			});
		}, timeout);
	};

	/**
	 * ��ʓ��e���X�V����SSE���J�n����B
	 */
	let startScreenUpdateListener = function() {
		let url = buildUrl('getOut.php', getQueryParams());

		_esScreen = new EventSource(url);
		_esScreen.onmessage = function(e) {
			// ���b�Z�[�W��M
			preProcess();

			try {
				onmessage(e.data);
			} finally {
				postProcess();
			}
		};
		_esScreen.onopen = function(e) {
			_isSSEOpen = true;
		};
	};

	/**
	 * ��ʓ��e���X�V����SSE���~����B
	 */
	let stopScreenUpdateListener = function() {
		if (_esScreen) {
			_esScreen.close();
			_esScreen = null;
		}
	};

	/**
	 * �p�����[�^���擾����B
	 */
	let getQueryParams = function() {
		let queryParams = {
			pid : $('#pid').val(),
			infname : $('#infname').val(),
			outfname : $('#outfname').val(),
		};

		return queryParams;
	};

	/**
	 * URL��g�ݗ��Ă�
	 */
	let buildUrl = function(url, queryParams) {
		let param = $.param(queryParams);

		if (-1 < url.indexOf('?')) {
			return url + '&' + param;
		} else {
			return url + '?' + param;
		}
	};

	/**
	 * ��ʂ̗v�f�����ւ���B
	 */
	this.screenReplace = function(resultTxt, isChgInput) {
		let $tarScreen = $('.screen');
		let $chgScreen = $('<div>');
		$chgScreen.html(resultTxt);

		if (!checkTimestamp($chgScreen)) {
			return;
		}

		if (!checkStatus($chgScreen)) {
			return;
		}

		// �擾�s�̂��ꂩ��
		replaceScreenElement($tarScreen, $chgScreen, isChgInput);
	};

	/**
	 * ��ʃf�[�^�̃^�C���X�^���v���`�F�b�N����B
	 */
	let checkTimestamp = function($chgScreen) {
		let $newDataTimestamp = $chgScreen.find('#dataTimestamp');
		let newDataTimestamp = parseFloat($newDataTimestamp.val());

		// �ǂݎ���ɗv�f�͍폜
		$newDataTimestamp.remove();

		if (_dataTimestamp <= newDataTimestamp) {
			// �f�[�^���V�����Ȃ��Ă���ꍇ
			// �p�����[�^���M���SSE�œ������e���߂��Ă��邱�Ƃ�����A�p�����[�^���M��͕K�����͗���
			// �����������s���K�v������̂Ń^�C���X�^���v��=(�C�R�[��)�����e���ă`�F�b�N����B
			_dataTimestamp = newDataTimestamp;
			return true;
		}

		// �^�C���X�^���v���Â��f�[�^�̏ꍇ�ł����͗������݂��邩�ۂ������͔��肵�Ă����B
		let $chgInputs = $chgScreen.find('.line > input[type="text"]');
		if (0 < $chgInputs.length) {
			_lastHasInputTime = new Date().getTime();
		}

		return false;
	};

	/**
	 * �����̃X�e�[�^�X�ɍ��킹�ăX�e�[�^�X���X�V���������I������B
	 */
	let checkStatus = function($chgScreen) {
		// $chgScreen�̒�����error�T���ĕ\��
		let $errors = $chgScreen.find('.error');
		if (0 < $errors.length) {
			// �G���[����
			let errorValue = $errors[0].value;
			let $status1 = $('#status1');
			$status1.html('<span>' + errorValue + '</span>');
		}

		// status2Get�����݂��Ă���ꍇstatus2�ɑ������
		// $chgScreen�̒�����status2Get������
		let $status2 = $chgScreen.find('#status2');
		if (0 < $status2.length) {
			$("#status2").html("<span>" + $status2[0].value + "</span>");
			$status2.remove();
		}

		// status4Get�����݂��Ă���ꍇstatus4�ɑ������
		// $chgScreen�̒�����status4Get������
		let $status4 = $chgScreen.find('#status4');
		if (0 < $status4.length) {
			$("#status4").html("<span>" + $status4[0].value + "</span>");
			$status4.remove();
		}

		// parentStatusGet�����݂��Ă���ꍇparentStatus�ɑ������
		// $chgScreen�̒�����parentStatusGet������
		let $parentStatusGet = $chgScreen.find('.parentStatusGet');
		if (0 < $parentStatusGet.length) {
			$('#parentStatus').val($parentStatusGet.val());
			$parentStatusGet.remove();
		}

		// �v���Z�X���I�������ꍇ�͉�ʍX�V��SSE���~
		if (_that.isProcessEnd()) {
			stopScreenUpdateListener();
		}

		// �G���[���������Ă��Ȃ��ăv���Z�X���I�����Ă���ꍇ�͑O��ʂɖ߂�B(��ʂ��I������)
		if (_that.isProcessEnd() && !_that.hasError()) {
			_that.historyBack();
			return false;
		}

		return true;
	};

	/**
	 * ��ʓ��e���X�V����
	 */
	let replaceScreenElement = function($tarScreen, $chgScreen, isChgInput) {
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
			// ���͗�����
			_lastHasInputTime = new Date().getTime();
		}

		// �v�f�̓���ւ����@������
		let isLineChange = false;
		if (isChgInput) {
			// ���͓��e���M��͕K���T�[�o�[����ԋp���ꂽ���e�ł��ׂčĕ\������
			isLineChange = false;
		} else {
			// ���݂Ǝ��̉�ʂɓ������͍��ڂ�����ꍇ�̂ݍs�P�ʂŗv�f�̓���ւ����s��
			if (0 < $tarInputs.length && 0 < $chgInputs.length) {
				let tarInput = $tarInputs[0];
				let chgInput = $chgInputs[0];
				if (tarInput.name == chgInput.name) {
					let tarColumns = tarInput.className.match(/\s+f[0-9]+/i);
					let chgColumns = chgInput.className.match(/\s+f[0-9]+/i);

					if (tarColumns[0] == chgColumns[0]) {
						// name�������ł��N���X�̃t�B�[���h��������
						isLineChange = true;
					}
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
			let $tarLines = $tarScreen.children();
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

				// ���͗v�f�̂���s�͓��͗v�f�ȊO���폜���ē���ւ����s��
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
			_isInputStop = false;
		} else if (isChgStop) {
			// STOP�����ꂽ
			_isInputStop = true;
		}

		// ���[�_��
		let $errBuz = $chgScreen.find('#err-buz');
		if (0 < $errBuz.length) {
			_buzzer.start(parseInt($errBuz.val()));
		}
		// ���̂�
		let $infoBuz = $chgScreen.find('#info-buz');
		if (0 < $infoBuz.length) {
			_buzzer.start(parseInt($infoBuz.val()));
		}

		// �t�H�[�J�X�����킹��
		setInputFocus();
	};

	/**
	 * ��M�������b�Z�[�W����������B
	 */
	let onmessage = function(data) {
		// ��ʓ��e�̍X�V
		_that.screenReplace(data, false);
	};
	/**
	 * �_�ŕ\���ݒ���s���B
	 */
	let setBlinkToElement = function($chgScreen) {
		let targetArray = $chgScreen.find('.blink');
		let ii = 0;
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
	};

	/**
	 * ���͗��Ƀt�H�[�J�X�����킹��B
	 */
	let setInputFocus = function() {
		if (document.activeElement.id != 'skSelect') {
			let $active = $(document.activeElement);
			if (!$active.hasClass('nextinput')) {
				$('input.nextinput:last').focus();
			}
		}
	};

	/**
	 * ���͓��e�ύX���̏������s���B
	 */
	this.changeInput = function() {
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

			// ���̓`�F�b�N(�X�e�[�^�X�̏�������)���s��
			elementInpCheck($input[0]);
		}
	}
}

/**
 * �L�[���͐���A�T�[�o�[�ւ̃p�����[�^���M���s���B
 */
var InputKeyControl = function(screen, buzzer) {

	let _screen = screen;

	/**
	 * �u�U�[����
	 */
	let _buzzer = buzzer;

	/**
	 * �L�[���͂̃o�b�t�@
	 */
	let _keyBuffer = new Array();

	/**
	 * Firefox�̓��{����͊m�蔻��̂��߂̃t���O
	 * Firefox�̏ꍇ�͓��{����͂̃L�[�C�x���g�͓��{��m�莞�̃G���^�[�L�[��keyup���ɂ݂̂ɔ�������B
	 * ����������{����͂��ꂽ���ۂ��𔻒肷��B
	 */
	let _isKeydown = false;

	/**
	 * ��������public���\�b�h�ɃA�N�Z�X���邽�߂̕ϐ�
	 */
	let _that = this;

	/**
	 * �p�����[�^���M�p��Ajax�g��
	 */
	let ajaxSendParam = function(arg) {
		let opt = $.extend({}, $.ajaxSettings, arg);

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
			return function(data, textStatus, jqXHR) {
				if (func) {
					func(data, textStatus, jqXHR);
				}
			};
		})(opt.success);

		// �G���[
		opt.error = (function(func) {
			return function(jqXHR, textStatus, errorThrown) {
				alert('BackGround connect Error' + textStatus + ':'
						+ errorThrown.message);
				console.log('BackGround connect Error' + textStatus + ':'
						+ errorThrown.message);

				if (func) {
					func(jqXHR, textStatus, errorThrown);
				}
			};
		})(opt.error);

		// ����
		opt.complete = (function(func) {
			return function(jqXHR, textStatus) {
				if (func) {
					func(jqXHR, textStatus);
				}

				// �㏈��
				postProcess();
			};
		})(opt.complete);

		return $.ajax(opt);
	};

	/**
	 * �L�[���͂̃C�x���g���󂯕t����
	 */
	this.bind = function() {

		// �L�[�_�E���C�x���g
		$(document).keydown(keydown);

		// �L�[�A�b�v�C�x���g
		if (!_screen.isMenuScreen()) {
			$(document).keyup(keyup);
		}
	};

	/**
	 * �L�[�_�E�����̏���
	 */
	let keydown = function(e) {
		// �G���[���b�Z�[�W���\���� ���v���Z�X�I�����̓L�[���얳��
		if (_screen.isProcessEnd() && _screen.hasError()) {
			return false;
		}

		// ��ʂ̎�ނɂ���ď����𕪊�
		if (_screen.isMenuScreen()) {
			// ���j���[���

			// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
			if (screenSwitch(e)) {
				return false;
			}

			// ����ȓ���������L�[�𖳌�������(F1�`F12�AALT�AESC)
			if (e.key.match(/F[0-9]{1,2}/) || e.key == 'Alt'
					|| e.key == 'Escape') {
				return false;
			}

			return true;
		}

		// ���͌n�̉��

		// ���{����͔���p
		if (isFirefox && e.key == 'Enter') {
			_isKeydown = true;
		}

		if (processQueue.length) {
			// �������̏ꍇ�͂��ׂăo�b�t�@�����O
			_keyBuffer.push(e);

			return false;
		} else {
			// �o�b�t�@�����O�������͂��c���Ă���ꍇ�͓��͂��ꂽ�L�[���o�b�t�@�̖����ɒǉ����A�L�[����𕜌�
			if (0 < _keyBuffer.length) {
				_keyBuffer.push(e);
				_that.keyBufferSimulate();

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

			// ���͗������݂��Ȃ��ꍇ�͉�ʏ��������̃^�C�~���O�ł܂����͍��ڂ����݂��Ȃ������Ȃ̂ŃL�[���͂��o�b�t�@
			// (���͗������݂��Ȃ���ʂł����͂��󂯕t�����ʂ͂��邪�A���̏ꍇ����x�o�b�t�@�ɓ���Ă��珈�����s���B)
			let $targets = $('input.nextinput');
			if ($targets.length == 0) {
				_keyBuffer.push(e);
				_that.keyBufferSimulate();

				return false;
			}

			// �ȍ~�͓��͗������݂���Ƃ��������s����鏈��

			// F11�`F12�͓��͂ɂ͎g��Ȃ�
			if (e.key == 'F11' || e.key == 'F12') {
				return false;
			}

			let target = $targets[0];

			// �ʏ�̕������͂��ۂ�
			if (isKeyInputValue(e)) {
				// �����}��
				if (insertKey(e, target)) {
					return false;
				}

				return true;
			}

			// DELETE
			if (e.key == 'Delete') {
				// DELETE�L�[�̓o�b�N�X�y�[�X�L�[�Ɠ���������s��
				backspaceKey(target);
				return false;
			}

			// �T�[�o�[�ɑ��M����X�e�[�^�X�l���擾����
			let statusValue = getStatusValue(e);

			// �T�[�o�[�ɑ��M����L�[����Ȃ��ꍇ
			if (statusValue == null) {
				return true;
			}

			let inputValue = '';
			// �o�b�N�X�y�[�X�A�G�X�P�[�v�͓��͓��e�𑗐M���Ȃ��̂Ń`�F�b�N���Ȃ�
			if (e.key != 'Backspace' && e.key != 'Escape') {
				// ���͍��ڂ̃`�F�b�N���s��
				let isTruncate = isTruncateValueKey(e);
				if (!elementInpCheck(target, isTruncate)) {
					return false;
				}
				// ���̓t�H�[�}�b�g�i�����Ή��j
				inputValue = elementInpFormat(target, isTruncate);
			}

			ajaxSendParam({
				data : getKeyParams(inputValue, statusValue),
				success : function(msg) {
					if (e.key == 'Enter' || e.key == 'Escape') {
						// �u�U�[���~
						_buzzer.stop();
					}
					_screen.screenReplace(msg, true);
				}
			});

			return false;
		}

		return true;
	};

	/**
	 * �L�[�A�b�v���̏���
	 */
	let keyup = function(e) {
		let isProcess = false;

		if (isFirefox && e.key == 'Enter') {
			if (!_isKeydown) {
				// ���{����͊m��̃G���^�[
				isProcess = true;
			}
			_isKeydown = false;
		}

		// �v���Z�X���I�����ăG���[���\������Ă���ꍇ�͑O��ʂ�
		// �T�[�o�[�ɑ����Ă��ǂ����悤���ł��Ȃ��̂Ńo�b�t�@�����O�����ɑ����ɏI��
		if (e.key == 'Escape') {
			if (_screen.isProcessEnd() && _screen.hasError()) {
				_screen.historyBack();
				return false;
			}
		}

		// ���{����͂����ꂽ�ꍇ�͓��͏������t�]���Ă��܂��̂Ńo�b�t�@�̓N���A
		if (isProcess) {
			keyBufferClear();
		}

		_screen.changeInput();

		return true;
	};

	/**
	 * �o�b�t�@�����O�����L�[���͂����s����
	 */
	this.keyBufferSimulate = function() {
		// �������̏ꍇ�͉����������Ȃ�
		if (0 < processQueue.length) {
			return;
		}

		// �o�b�t�@�ɉ����Ȃ��ꍇ�͉������Ȃ�
		if (_keyBuffer.length == 0) {
			return;
		}

		// �G���[���b�Z�[�W���\���� ���v���Z�X�I�����̓L�[���얳���Ȃ��߃o�b�t�@�N���A
		if (screen.isProcessEnd() && screen.hasError()) {
			keyBufferClear();
			return;
		}

		let $targets = $('input.nextinput');

		if (0 < $targets.length) {
			// ���͗�������ꍇ�͂܂Ƃ߂đ������̂͑���
			if (sendBatchParam($targets[0])) {
				return;
			}

			// �o�b�t�@�ɉ����Ȃ��Ȃ����ꍇ�͉������Ȃ�
			if (_keyBuffer.length == 0) {
				return;
			}
		}

		while (true) { // �_�~�[�u���b�N
			let e = _keyBuffer.shift();

			// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
			if (screenSwitch(e)) {
				break;
			}

			// CTL+ANY�L�[�̏���
			if (funcSpecialKey(e)) {
				break;
			}

			// ���͗����Ȃ��@�\ or ��ʂ̏��������^�C�~���O�ɂ���ē��͗����Ȃ��ꍇ�̏���
			if ($targets.length == 0) {

				let elapsedTime = 0;
				if (!_screen.getLastHasInputTime()) {
					// ��ʂ��J����1�x�����͗������݂��Ȃ�
					elapsedTime = -1;
				} else {
					// ���͗������݂��Ȃ��Ȃ��Ă���̌o�߃~���b
					elapsedTime = new Date().getTime()
							- _screen.getLastHasInputTime();
				}

				// ��莞�ԓ��͗������݂��Ȃ���Ԃ������Ă���ꍇ�͓��͗��Ȃ��̉�ʂƂ��Ĕ��f
				if (elapsedTime == -1
						|| NONE_INPUT_AREA_ELAPSED_TIME < elapsedTime) {
					// CTL+Function�L�[�̏���
					if (isKeyFunction(e)) {
						ajaxSendParam({
							// F + Function�L�[�̒l
							data : getKeyParams('F' + (e.keyCode - 111), ''),
							success : function(msg) {
								_screen.screenReplace(msg, true);
							}
						});
						break;
					}

					// �T�[�o�[�ɑ��M����X�e�[�^�X�l���擾����
					let statusValue = getStatusValue(e);

					// �T�[�o�[�ɑ��M����L�[����Ȃ��ꍇ�͎̂Ă�
					if (statusValue == null) {
						break;
					}

					ajaxSendParam({
						data : getKeyParams('', statusValue),
						success : function(msg) {
							if (e.key == 'Enter' || e.key == 'Escape') {
								// �u�U�[���~
								_buzzer.stop();
							}
							_screen.screenReplace(msg, true);
						}
					});

					if (_screen.getLastHasInputTime()) {
						// �A���Ńo�b�t�@�����������Ǝ��Ԃ̍X�V�̂ق����x���Ȃ�K����̏����ɓ����Ă��܂��̂ł����Ŏ��Ԃ��X�V���Ă����B
						_screen.setLastHasInputTime(new Date().getTime());
					}
				} else {
					// ���͗��͂����ʂ����A��ʏ��������̃^�C�~���O�ł܂����͗������݂��Ȃ������̏ꍇ�͓��͂��o�b�t�@�ɖ߂��čĎ��s
					_keyBuffer.unshift(e);
				}

				break;
			}

			if (_screen.getLastHasInputTime()) {
				// ���̎��_�ł͓��͗������݂���̂Ŏ��Ԃ��X�V���Ă����B
				_screen.setLastHasInputTime(new Date().getTime());
			}

			// �ȍ~�͓��͗������݂���Ƃ��������s����鏈��

			// F11�`F12�͓��͂ɂ͎g��Ȃ�
			if (e.key == 'F11' || e.key == 'F12') {
				break;
			}

			let target = $targets[0];

			// �ʏ�̕������͂��ۂ�
			if (isKeyInputValue(e)) {
				// �����}��
				if (insertKey(e, target)) {
					break;
				}

				// �����ɒǉ�
				appendKey(e, target);

				screen.changeInput();

				break;
			}

			// DELETE
			if (e.key == 'Delete') {
				// DELETE�L�[�̓o�b�N�X�y�[�X�L�[�Ɠ���������s��
				backspaceKey(target);
				break;
			}

			// �J�[�\���ړ�
			if (e.key == 'ArrowRight') {
				arrowRightKey(target);
				break;
			}
			if (e.key == 'ArrowLeft') {
				arrowLeftKey(target);
				break;
			}

			// �T�[�o�[�ɑ��M����X�e�[�^�X�l���擾����
			let statusValue = getStatusValue(e);

			// �T�[�o�[�ɑ��M����L�[����Ȃ��ꍇ
			if (statusValue == null) {
				break;
			}

			let inputValue = '';
			// �o�b�N�X�y�[�X�A�G�X�P�[�v�͓��͓��e�𑗐M���Ȃ��̂Ń`�F�b�N���Ȃ�
			if (e.key != 'Backspace' && e.key != 'Escape') {
				// ���͍��ڂ̃`�F�b�N���s��
				let isTruncate = isTruncateValueKey(e);
				if (!elementInpCheck(target, isTruncate)) {
					break;
				}
				// ���̓t�H�[�}�b�g�i�����Ή��j
				inputValue = elementInpFormat(target, isTruncate);
			}

			ajaxSendParam({
				data : getKeyParams(inputValue, statusValue),
				success : function(msg) {
					if (e.key == 'Enter' || e.key == 'Escape') {
						// �u�U�[���~
						_buzzer.stop();
					}
					_screen.screenReplace(msg, true);
				}
			});

			break;
		}

		// �ċA���s
		setTimeout(_that.keyBufferSimulate, KEY_BUFFER_SIMULATE_INTERVAL);
	};

	/**
	 * �L�[�o�b�t�@�����O�������͓��e�̂����T�[�o�[�ɑ��M�ł���L�[���܂Ƃ߂đ��M����B
	 */
	let sendBatchParam = function(inputTarget) {
		let sendKeyParam = new Array();
		let isEnterKey = false;
		let isEscKey = false;
		// ���͒l����
		let isInput = 0 < inputTarget.value.length;

		// ���̓`�F�b�N���s�v�ȃL�[�̂݃p�����[�^�ɃZ�b�g���đ���
		for (let i = 0; i < _keyBuffer.length; i++) {
			let e = _keyBuffer[i];

			// CTL+ANY�L�[�̏���(��ʐؗ��֌W)
			if (isScreenSwitch(e)) {
				break;
			}

			// CTL+ANY�L�[�̏���
			if (isFuncSpecialKey(e)) {
				break;
			}

			// CTL+Function�L�[�̏���
			if (isKeyFunction(e)) {
				break;
			}

			// F11�`F12�͓��͂ɂ͎g��Ȃ��̂ŃX�L�b�v
			if (e.key == 'F11' || e.key == 'F12') {
				_keyBuffer.splice(i, 1);
				i--;
				continue;
			}

			// �ʏ�̓��͕������ۂ�
			if (isKeyInputValue(e)) {
				break;
			}

			// DELETE
			if (e.key == 'Delete') {
				break;
			}

			// �J�[�\���ړ�
			if (e.key == 'ArrowRight') {
				break;
			}
			if (e.key == 'ArrowLeft') {
				break;
			}

			// �T�[�o�[�ɑ��M����X�e�[�^�X�l���擾����
			let statusValue = getStatusValue(e);

			// �����ȊO�̃T�[�o�[���M�s�v�ȃL�[�̏ꍇ�͉������Ȃ��Ƃ������ƂȂ̂ŃX�L�b�v����
			if (statusValue == null) {
				_keyBuffer.splice(i, 1);
				i--;
				continue;
			}

			let inputValue = '';

			// ���̓`�F�b�N���s�v�ȃL�[�̂݃p�����[�^�ɃZ�b�g
			if (!isInput || e.key == 'Backspace' || e.key == 'Escape') {
				if (e.key == 'Escape') {
					isEscKey = true;
				} else if (e.key == 'Enter') {
					isEnterKey = true;
				}

				sendKeyParam.push({
					sendValue : inputValue,
					statusValue : statusValue,
				});

				_keyBuffer.splice(i, 1);
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
					if (isEscKey || isEnterKey) {
						// �u�U�[���~
						_buzzer.stop();
					}
					_screen.screenReplace(msg, true);
				}
			});

			return true;
		}

		return false;
	}

	/**
	 * �L�[�o�b�t�@�����O���N���A
	 */
	let keyBufferClear = function() {
		_keyBuffer.length = 0;
	}

	/**
	 * ���͂��ꂽ�L�[�����͕������ۂ�
	 */
	let isKeyInputValue = function(e) {
		// CTL or ALT��������Ă���
		if (e.ctrlKey == true || e.altKey == true) {
			return false;
		}

		// �L�[��2�����ȏ�ŕ\�����
		if (1 < e.key.length) {
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
	 * ���͂��ꂽ�L�[��Function�L�[���ۂ�
	 */
	let isKeyFunction = function(e) {
		// CTL + F1�`F12
		if (e.ctrlKey == true) {
			if (e.key.match(/F[0-9]{1,2}/)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * ������}������B �J�[�\���ʒu�������̏ꍇ�͐���s�v�Ȃ̂ŉ������Ȃ��B(false��ԋp)
	 */
	let insertKey = function(e, field) {
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
		field.selectionEnd = field.selectionStart;

		return true;
	}

	/**
	 * �����𖖔��ɒǉ�����B
	 */
	let appendKey = function(e, field) {
		let fieldValue = field.value;

		// maxLength�𒴂���ꍇ�͉������Ȃ�
		if (field.maxLength <= fieldValue.length) {
			return false;
		}
		let newValue = fieldValue + e.key;

		// �V����������ݒ肵�ăJ�[�\���ʒu���Đݒ肷��
		field.value = newValue;
		field.selectionStart = newValue.length;
		field.selectionEnd = field.selectionStart;

		return true;
	}

	/**
	 * �o�b�N�X�y�[�X�L�[�̓�����s���B
	 */
	let backspaceKey = function(field) {
		let startPos = field.selectionStart;
		let fieldValue = field.value;

		if (0 < startPos) {
			let newValue = fieldValue.slice(0, startPos - 1)
					+ fieldValue.slice(startPos);
			field.value = newValue;
			field.selectionStart = startPos - 1;
			field.selectionEnd = field.selectionStart;
		}
	}

	/**
	 * �E���L�[�̓�����s���B
	 */
	let arrowRightKey = function(field) {
		if (field.selectionStart < field.value.length) {
			field.selectionStart = field.selectionStart + 1;
			field.selectionEnd = field.selectionStart;
		}
	}

	/**
	 * �����L�[�̓�����s���B
	 */
	let arrowLeftKey = function(field) {
		if (0 < field.selectionStart) {
			field.selectionStart = field.selectionStart - 1;
			field.selectionEnd = field.selectionStart;
		}
	}

	/**
	 * �T�[�o�[�ɑ��M����X�e�[�^�X�̒l���擾����B �T�[�o�[�ɑ��M����L�[�łȂ��ꍇ��null��Ԃ��B
	 */
	let getStatusValue = function(e) {

		let statusValue = null;

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
			statusValue = 'esc';
			break;
		case 'Tab':
			statusValue = 's';
			break;
		case 'Backspace':
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
			// Firefox�ł͔������Ȃ�
			statusValue = null;
		default:
			statusValue = null;
		}

		return statusValue;
	}

	/**
	 * ���͂��ꂽ�������J�[�\���ʒu�Ő؂�̂Ă�L�[���ۂ��𔻒f����B
	 */
	let isTruncateValueKey = function(e) {
		switch (e.key) {
		case 'Enter':
			if (e.shiftKey == true) {
				break;
			}
			return true;
		}

		return false;
	}

	/**
	 * �L�[���͂̃p�����[�^���擾����B
	 */
	let getKeyParams = function(value, statusValue) {
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
	let getKeyBatchParams = function(sendKeyParam) {
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
};

/**
 * �u�U�[������s��
 */
var BuzzerControl = function() {
	let _buzzerCounter = -1;
	let _buzzerTimeoutId = -1;

	let buzzer = function(soundLength) {
		if (-1 < _buzzerTimeoutId) {
			clearTimeout(_buzzerTimeoutId);
		}

		if (_buzzerCounter++ < soundLength) {
			// base64�������\��t��(���̒����������ŃR���g���[������K�v�L��H
			let base64 = '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'
			// datauri scheme �`���ɂ��� Audio �I�u�W�F�N�g�𐶐����܂�
			let sound = new Audio('data:audio/wav;base64,' + base64);
			// ����炵�܂�
			sound.play();
			/*
			 * var msg = '�G���[���������܂����B\n'; msg += '�Ǘ��҂ɘA�����Ă��������B\n'; msg +=
			 * soundLength + ''; alert(msg); BuzzerFlg = 1;
			 */

			// ���̃^�C�}�[���Z�b�g
			_buzzerTimeoutId = setTimeout(function() {
				buzzer(soundLength);
			}, BUZZER_INTERVAL);
		}
	};

	/**
	 * �u�U�[���~�߂�
	 */
	this.stop = function() {
		if (this.isBuzzer()) {
			clearTimeout(_buzzerTimeoutId);
			_buzzerCounter = -1;
		}
	};

	/**
	 * �u�U�[��炷
	 */
	this.start = function(soundLength) {
		// ���ɍĐ����Ă���ꍇ�͏㏑���Đ��͂��Ȃ�
		if (!this.isBuzzer()) {
			_buzzerCounter = 0;
			buzzer(soundLength);
		}
	};

	/**
	 * �u�U�[��炵�Ă��邩�ۂ�
	 */
	this.isBuzzer = function() {
		return -1 < _buzzerCounter;
	};
}

var buzzer = new BuzzerControl();
var screen = new ScreenControl(buzzer);
var inputKey = new InputKeyControl(screen, buzzer);

addEvent('load', window, screen.init);
addEvent('load', window, inputKey.bind);
