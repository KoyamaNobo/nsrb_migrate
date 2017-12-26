/**
 * ブザー音を鳴らす間隔(msec)
 */
const BUZZER_INTERVAL = 600;
/**
 * セッション継続用ポーリング間隔(msec)
 */
const SESSION_ALIVE_INTERVAL = 1000 * 60 * 5;
/**
 * キー入力を再現する際の再現間隔(msec)
 */
const KEY_BUFFER_SIMULATE_INTERVAL = 10;
/**
 * 入力欄なしの画面と判定する時間(msec) 。ここで設定した時間、画面にnextinputの入力欄が表示されない場合は入力欄なしの画面として判断する。
 */
const NONE_INPUT_AREA_ELAPSED_TIME = 3000;

/**
 * ブラウザがFirefoxか否か
 */
var isFirefox = (function() {
	var userAgent = window.navigator.userAgent.toLowerCase();
	return userAgent.indexOf('firefox') != -1;

})();

/**
 * 処理中を表すキュー。 キューにデータが存在する場合はキー入力をブロックしてバッファリングする。
 * ただし、日本語入力をブロックすることはできないので、ローマ字入力と日本語入力とでは 動作が異なることになる。
 */
var processQueue = new Array();

/**
 * 画面の書き換えが発生する処理の前処理を行う。
 */
var preProcess = function() {
	processQueue.push('');
}

/**
 * 画面の書き換えが発生する処理の後処理を行う。
 */
var postProcess = function() {
	processQueue.pop();
	inputKey.keyBufferSimulate();
}

/**
 * 画面表示制御を行う。
 */
var ScreenControl = function(buzzer) {

	/**
	 * STOP状態を表すフラグ
	 */
	let _isInputStop = false;

	/**
	 * 画面データのタイムスタンプ。 古いデータをスキップするための判定用
	 */
	let _dataTimestamp = 0;

	/**
	 * 画面をSSEで更新するためのイベントソース
	 */
	let _esScreen;

	/**
	 * 内部からpublicメソッドにアクセスするための変数
	 */
	let _that = this;

	/**
	 * 最後に入力欄が存在したときの時間 (入力欄が存在する画面か否かを判断するために使用する)
	 */
	let _lastHasInputTime;

	/**
	 * SSEでの接続が確立したか否か
	 */
	let _isSSEOpen = false;

	/**
	 * ブザー操作
	 */
	let _buzzer = buzzer;

	/**
	 * メニュー画面か否か判定する。
	 */
	this.isMenuScreen = function() {
		let infname = $('#infname');
		// menu等の場合infnameがない
		if (infname.length == 0) {
			return true;
		}

		return false;
	}

	/**
	 * 前画面に戻る。
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
	 * サーバーのプロセスが終了しているか否か判定する。
	 */
	this.isProcessEnd = function() {
		let $parentStatus = $('#parentStatus');

		if (0 < $parentStatus.length) {
			if ($parentStatus.val() == 'end') {
				// プロセス終了
				return true;
			}
		}

		return false;
	}

	/**
	 * エラー内容を表示しているか否か判定する。
	 */
	this.hasError = function() {
		let html = $('#status1').html();
		return 0 < $.trim(html.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g, '')).length;
	}

	/**
	 * 最後に入力項目が存在した時間を取得する。
	 */
	this.getLastHasInputTime = function() {
		return _lastHasInputTime;
	}

	/**
	 * 最後に入力項目が存在した時間を設定する。
	 */
	this.setLastHasInputTime = function(lastTime) {
		_lastHasInputTime = lastTime;
	}

	/**
	 * 初期表示処理を行う。
	 */
	this.init = function() {
		// メニュー系の画面の場合は何も処理をしない
		if (_that.isMenuScreen()) {
			return;
		}

		// PIDが取得できない場合は前画面に戻り何も処理をしない
		if ($('#pid').length == 0) {
			historyBack();
			return;
		}

		// Ajaxで画面データの取得を行う
		screenUpdatePolling();

		// 画面データ取得用のSSEを開始する
		startScreenUpdateListener();

		// 入力欄にフォーカスを合わせる
		setInputFocus();
		$(document).on('focusout', function(e) {
			// 入力欄からフォーカスが外れないようにフォーカスが外れた際の再設定処理を行う。
			// focusoutの中でフォーカスの再設定ができないためsetTimeoutでタイミングをずらす。
			setTimeout(function() {
				setInputFocus();
			}, 1);
		});

		// 画面遷移前にSSEの接続を切断する
		// 明示的に切断しなくても問題ないがFirefoxだと強制切断された旨のメッセージがコンソールに表示されるため
		$(window).on('beforeunload', function() {
			stopScreenUpdateListener();
		});
	};

	/**
	 * Ajaxで画面データの取得を行う。(ポーリング処理)
	 */
	let screenUpdatePolling = function() {
		// SSEの接続が確立するまでは画面書き換え目的で。接続確立後はセッションタイムアウトを起こさない目的でポーリング間隔をかえる。
		// SSEは接続までにタイムラグがあるので、初めはAjaxで画面データを取得する。
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
	 * 画面内容を更新するSSEを開始する。
	 */
	let startScreenUpdateListener = function() {
		let url = buildUrl('getOut.php', getQueryParams());

		_esScreen = new EventSource(url);
		_esScreen.onmessage = function(e) {
			// メッセージ受信
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
	 * 画面内容を更新するSSEを停止する。
	 */
	let stopScreenUpdateListener = function() {
		if (_esScreen) {
			_esScreen.close();
			_esScreen = null;
		}
	};

	/**
	 * パラメータを取得する。
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
	 * URLを組み立てる
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
	 * 画面の要素を入れ替える。
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

		// 取得行のいれかえ
		replaceScreenElement($tarScreen, $chgScreen, isChgInput);
	};

	/**
	 * 画面データのタイムスタンプをチェックする。
	 */
	let checkTimestamp = function($chgScreen) {
		let $newDataTimestamp = $chgScreen.find('#dataTimestamp');
		let newDataTimestamp = parseFloat($newDataTimestamp.val());

		// 読み取り後に要素は削除
		$newDataTimestamp.remove();

		if (_dataTimestamp <= newDataTimestamp) {
			// データが新しくなっている場合
			// パラメータ送信後とSSEで同じ内容が戻ってくることがあり、パラメータ送信後は必ず入力欄の
			// 書き換えも行う必要があるのでタイムスタンプの=(イコール)も許容してチェックする。
			_dataTimestamp = newDataTimestamp;
			return true;
		}

		// タイムスタンプが古いデータの場合でも入力欄が存在するか否かだけは判定しておく。
		let $chgInputs = $chgScreen.find('.line > input[type="text"]');
		if (0 < $chgInputs.length) {
			_lastHasInputTime = new Date().getTime();
		}

		return false;
	};

	/**
	 * 処理のステータスに合わせてステータスを更新＆処理を終了する。
	 */
	let checkStatus = function($chgScreen) {
		// $chgScreenの中からerror探して表示
		let $errors = $chgScreen.find('.error');
		if (0 < $errors.length) {
			// エラーあり
			let errorValue = $errors[0].value;
			let $status1 = $('#status1');
			$status1.html('<span>' + errorValue + '</span>');
		}

		// status2Getが存在している場合status2に代入して
		// $chgScreenの中からstatus2Getを消す
		let $status2 = $chgScreen.find('#status2');
		if (0 < $status2.length) {
			$("#status2").html("<span>" + $status2[0].value + "</span>");
			$status2.remove();
		}

		// status4Getが存在している場合status4に代入して
		// $chgScreenの中からstatus4Getを消す
		let $status4 = $chgScreen.find('#status4');
		if (0 < $status4.length) {
			$("#status4").html("<span>" + $status4[0].value + "</span>");
			$status4.remove();
		}

		// parentStatusGetが存在している場合parentStatusに代入して
		// $chgScreenの中からparentStatusGetを消す
		let $parentStatusGet = $chgScreen.find('.parentStatusGet');
		if (0 < $parentStatusGet.length) {
			$('#parentStatus').val($parentStatusGet.val());
			$parentStatusGet.remove();
		}

		// プロセスが終了した場合は画面更新のSSEを停止
		if (_that.isProcessEnd()) {
			stopScreenUpdateListener();
		}

		// エラーが発生していなくてプロセスが終了している場合は前画面に戻る。(画面を終了する)
		if (_that.isProcessEnd() && !_that.hasError()) {
			_that.historyBack();
			return false;
		}

		return true;
	};

	/**
	 * 画面内容を更新する
	 */
	let replaceScreenElement = function($tarScreen, $chgScreen, isChgInput) {
		// lineクラスを持つ要素の子のinput要素を抽出
		let $tarInputs = $tarScreen.find('.line > input[type="text"]');
		let $chgInputs = $chgScreen.find('.line > input[type="text"]');

		// STOP判定。画面を書き換える前に判定しておく。
		let isTarStop = false;
		let isChgStop = false;
		if (0 < $tarInputs.length) {
			let tarInput = $tarInputs[0];
			isTarStop = tarInput.name == 'STOP';
		}
		if (0 < $chgInputs.length) {
			let chgInput = $chgInputs[0];
			isChgStop = chgInput.name == 'STOP';
			// 入力欄あり
			_lastHasInputTime = new Date().getTime();
		}

		// 要素の入れ替え方法を決定
		let isLineChange = false;
		if (isChgInput) {
			// 入力内容送信後は必ずサーバーから返却された内容ですべて再表示する
			isLineChange = false;
		} else {
			// 現在と次の画面に同じ入力項目がある場合のみ行単位で要素の入れ替えを行う
			if (0 < $tarInputs.length && 0 < $chgInputs.length) {
				let tarInput = $tarInputs[0];
				let chgInput = $chgInputs[0];
				if (tarInput.name == chgInput.name) {
					let tarColumns = tarInput.className.match(/\s+f[0-9]+/i);
					let chgColumns = chgInput.className.match(/\s+f[0-9]+/i);

					if (tarColumns[0] == chgColumns[0]) {
						// nameが同じでかつクラスのフィールド名が同じ
						isLineChange = true;
					}
				}
			}
		}

		// 点滅表示の状態に合わせる
		setBlinkToElement($chgScreen);

		// 要素の入れ替えを行う
		if (isLineChange) {
			// 行置き換え
			// 現在の画面から罫線要素の子を削除
			$tarScreen.children(':not(.line)').remove();
			// 同様に次の画面から罫線要素を削除。ただし、こちらは後でマージするので要素は残しておく。
			let $chgNotLines = $chgScreen.children('div:not(.line)');
			$chgNotLines.remove();

			// 現在の画面をベースに行単位の入れ替え処理を行う
			let $tarLines = $tarScreen.children();
			for (i = 0; i < $tarLines.length; i++) {
				let tarLine = $tarLines[i];
				// 次の画面から同じクラスを持つ要素を抽出
				// クラス名はスペースで繋がれて複数取得できるので、セレクタでクラス名のAND検索になるように.に置換して抽出
				let $chgLines = $chgScreen.children('.'
						+ tarLine.className.replace(' ', '.'));

				// 置き換え対象の要素が見つからない場合は何もせずにスキップ
				if ($chgLines.length == 0) {
					continue;
				}

				let $tarLine = $(tarLine);
				let $chgLine = $($chgLines[0]);

				// 入力要素がない行の場合は単純に内容を入れ替え
				if ($tarLine.find('input[type="text"]').length == 0) {
					$tarLine.html($chgLine.html());
					$chgLine.remove();
					continue;
				}

				// 入力要素のある行は入力要素以外を削除して入れ替えを行う
				$tarLine.children(':not(input[type="text"])').remove();
				$tarLine.append($chgLine.children(':not(input[type="text"])'));
				$chgLine.remove();
			}

			// 罫線要素をまとめて追加
			let addHtml = '';
			for (i = 0; i < $chgNotLines.length; i++) {
				addHtml += $chgNotLines[i].outerHTML;
			}
			$tarScreen.append(addHtml);
		} else {
			// 丸ごと置き換え
			$tarScreen.html($chgScreen.html());
		}

		// 画面を作り替えたら画面色再構成
		userSetting();

		// STOPを受信したときの処理
		if (isTarStop && isChgStop) {
			// STOP継続
		} else if (isTarStop) {
			// STOPが消えた
			_isInputStop = false;
		} else if (isChgStop) {
			// STOPが現れた
			_isInputStop = true;
		}

		// モーダル
		let $errBuz = $chgScreen.find('#err-buz');
		if (0 < $errBuz.length) {
			_buzzer.start(parseInt($errBuz.val()));
		}
		// 音のみ
		let $infoBuz = $chgScreen.find('#info-buz');
		if (0 < $infoBuz.length) {
			_buzzer.start(parseInt($infoBuz.val()));
		}

		// フォーカスを合わせる
		setInputFocus();
	};

	/**
	 * 受信したメッセージを処理する。
	 */
	let onmessage = function(data) {
		// 画面内容の更新
		_that.screenReplace(data, false);
	};
	/**
	 * 点滅表示設定を行う。
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
	 * 入力欄にフォーカスを合わせる。
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
	 * 入力内容変更時の処理を行う。
	 */
	this.changeInput = function() {
		let $input = $(document.activeElement);

		if (!$input.hasClass('nextinput')) {
			// アクティブな項目が入力欄でない場合はセレクタで入力欄を取得
			// ほとんどの場合でactiveElementが入力欄のはずなので、セレクタのコストを考慮して違う場合のみ取得
			$input = $('input.nextinput');
		}

		if (0 < $input.length) {
			if (0 < $input.val().length) {
				// 入力あり
				$input.css('background-color', $('#sbgcolor').val());
			} else {
				$input.css('background-color', '');
			}

			// 入力チェック(ステータスの書き換え)を行う
			elementInpCheck($input[0]);
		}
	}
}

/**
 * キー入力制御、サーバーへのパラメータ送信を行う。
 */
var InputKeyControl = function(screen, buzzer) {

	let _screen = screen;

	/**
	 * ブザー操作
	 */
	let _buzzer = buzzer;

	/**
	 * キー入力のバッファ
	 */
	let _keyBuffer = new Array();

	/**
	 * Firefoxの日本語入力確定判定のためのフラグ
	 * Firefoxの場合は日本語入力のキーイベントは日本語確定時のエンターキーがkeyup時にのみに発生する。
	 * そこから日本語入力されたか否かを判定する。
	 */
	let _isKeydown = false;

	/**
	 * 内部からpublicメソッドにアクセスするための変数
	 */
	let _that = this;

	/**
	 * パラメータ送信用のAjax拡張
	 */
	let ajaxSendParam = function(arg) {
		let opt = $.extend({}, $.ajaxSettings, arg);

		opt.type = 'POST';
		opt.url = 'param.php';

		// 前処理
		opt.beforeSend = (function(func) {
			return function(jqXHR) {
				// 前処理
				preProcess();

				if (func) {
					func(jqXHR);
				}
			};
		})(opt.beforeSend);

		// 処理成功
		opt.success = (function(func) {
			return function(data, textStatus, jqXHR) {
				if (func) {
					func(data, textStatus, jqXHR);
				}
			};
		})(opt.success);

		// エラー
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

		// 完了
		opt.complete = (function(func) {
			return function(jqXHR, textStatus) {
				if (func) {
					func(jqXHR, textStatus);
				}

				// 後処理
				postProcess();
			};
		})(opt.complete);

		return $.ajax(opt);
	};

	/**
	 * キー入力のイベントを受け付ける
	 */
	this.bind = function() {

		// キーダウンイベント
		$(document).keydown(keydown);

		// キーアップイベント
		if (!_screen.isMenuScreen()) {
			$(document).keyup(keyup);
		}
	};

	/**
	 * キーダウン時の処理
	 */
	let keydown = function(e) {
		// エラーメッセージが表示中 かつプロセス終了時はキー操作無効
		if (_screen.isProcessEnd() && _screen.hasError()) {
			return false;
		}

		// 画面の種類によって処理を分岐
		if (_screen.isMenuScreen()) {
			// メニュー画面

			// CTL+ANYキーの処理(画面切離関係)
			if (screenSwitch(e)) {
				return false;
			}

			// 特殊な動きをするキーを無効化する(F1～F12、ALT、ESC)
			if (e.key.match(/F[0-9]{1,2}/) || e.key == 'Alt'
					|| e.key == 'Escape') {
				return false;
			}

			return true;
		}

		// 入力系の画面

		// 日本語入力判定用
		if (isFirefox && e.key == 'Enter') {
			_isKeydown = true;
		}

		if (processQueue.length) {
			// 処理中の場合はすべてバッファリング
			_keyBuffer.push(e);

			return false;
		} else {
			// バッファリングした入力が残っている場合は入力されたキーをバッファの末尾に追加し、キー操作を復元
			if (0 < _keyBuffer.length) {
				_keyBuffer.push(e);
				_that.keyBufferSimulate();

				return false;
			}

			// CTL+ANYキーの処理(画面切離関係)
			if (screenSwitch(e)) {
				return false;
			}

			// CTL+ANYキーの処理
			if (funcSpecialKey(e)) {
				return false;
			}

			// 入力欄が存在しない場合は画面書き換えのタイミングでまだ入力項目が存在しないだけなのでキー入力をバッファ
			// (入力欄が存在しない画面でも入力を受け付ける画面はあるが、その場合も一度バッファに入れてから処理を行う。)
			let $targets = $('input.nextinput');
			if ($targets.length == 0) {
				_keyBuffer.push(e);
				_that.keyBufferSimulate();

				return false;
			}

			// 以降は入力欄が存在するときだけ実行される処理

			// F11～F12は入力には使わない
			if (e.key == 'F11' || e.key == 'F12') {
				return false;
			}

			let target = $targets[0];

			// 通常の文字入力か否か
			if (isKeyInputValue(e)) {
				// 文字挿入
				if (insertKey(e, target)) {
					return false;
				}

				return true;
			}

			// DELETE
			if (e.key == 'Delete') {
				// DELETEキーはバックスペースキーと同じ操作を行う
				backspaceKey(target);
				return false;
			}

			// サーバーに送信するステータス値を取得する
			let statusValue = getStatusValue(e);

			// サーバーに送信するキーじゃない場合
			if (statusValue == null) {
				return true;
			}

			let inputValue = '';
			// バックスペース、エスケープは入力内容を送信しないのでチェックしない
			if (e.key != 'Backspace' && e.key != 'Escape') {
				// 入力項目のチェックを行う
				let isTruncate = isTruncateValueKey(e);
				if (!elementInpCheck(target, isTruncate)) {
					return false;
				}
				// 入力フォーマット（少数対応）
				inputValue = elementInpFormat(target, isTruncate);
			}

			ajaxSendParam({
				data : getKeyParams(inputValue, statusValue),
				success : function(msg) {
					if (e.key == 'Enter' || e.key == 'Escape') {
						// ブザーを停止
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
	 * キーアップ時の処理
	 */
	let keyup = function(e) {
		let isProcess = false;

		if (isFirefox && e.key == 'Enter') {
			if (!_isKeydown) {
				// 日本語入力確定のエンター
				isProcess = true;
			}
			_isKeydown = false;
		}

		// プロセスが終了してエラーが表示されている場合は前画面へ
		// サーバーに送ってもどうしようもできないのでバッファリングせずに即座に終了
		if (e.key == 'Escape') {
			if (_screen.isProcessEnd() && _screen.hasError()) {
				_screen.historyBack();
				return false;
			}
		}

		// 日本語入力がされた場合は入力順序が逆転してしまうのでバッファはクリア
		if (isProcess) {
			keyBufferClear();
		}

		_screen.changeInput();

		return true;
	};

	/**
	 * バッファリングしたキー入力を実行する
	 */
	this.keyBufferSimulate = function() {
		// 処理中の場合は何も処理しない
		if (0 < processQueue.length) {
			return;
		}

		// バッファに何もない場合は何もしない
		if (_keyBuffer.length == 0) {
			return;
		}

		// エラーメッセージが表示中 かつプロセス終了時はキー操作無効なためバッファクリア
		if (screen.isProcessEnd() && screen.hasError()) {
			keyBufferClear();
			return;
		}

		let $targets = $('input.nextinput');

		if (0 < $targets.length) {
			// 入力欄がある場合はまとめて送れるものは送る
			if (sendBatchParam($targets[0])) {
				return;
			}

			// バッファに何もなくなった場合は何もしない
			if (_keyBuffer.length == 0) {
				return;
			}
		}

		while (true) { // ダミーブロック
			let e = _keyBuffer.shift();

			// CTL+ANYキーの処理(画面切離関係)
			if (screenSwitch(e)) {
				break;
			}

			// CTL+ANYキーの処理
			if (funcSpecialKey(e)) {
				break;
			}

			// 入力欄がない機能 or 画面の書き換えタイミングによって入力欄がない場合の処理
			if ($targets.length == 0) {

				let elapsedTime = 0;
				if (!_screen.getLastHasInputTime()) {
					// 画面を開いて1度も入力欄が存在しない
					elapsedTime = -1;
				} else {
					// 入力欄が存在しなくなってからの経過ミリ秒
					elapsedTime = new Date().getTime()
							- _screen.getLastHasInputTime();
				}

				// 一定時間入力欄が存在しない状態が続いている場合は入力欄なしの画面として判断
				if (elapsedTime == -1
						|| NONE_INPUT_AREA_ELAPSED_TIME < elapsedTime) {
					// CTL+Functionキーの処理
					if (isKeyFunction(e)) {
						ajaxSendParam({
							// F + Functionキーの値
							data : getKeyParams('F' + (e.keyCode - 111), ''),
							success : function(msg) {
								_screen.screenReplace(msg, true);
							}
						});
						break;
					}

					// サーバーに送信するステータス値を取得する
					let statusValue = getStatusValue(e);

					// サーバーに送信するキーじゃない場合は捨てる
					if (statusValue == null) {
						break;
					}

					ajaxSendParam({
						data : getKeyParams('', statusValue),
						success : function(msg) {
							if (e.key == 'Enter' || e.key == 'Escape') {
								// ブザーを停止
								_buzzer.stop();
							}
							_screen.screenReplace(msg, true);
						}
					});

					if (_screen.getLastHasInputTime()) {
						// 連続でバッファが処理されると時間の更新のほうが遅くなり必ず上の処理に入ってしまうのでここで時間を更新しておく。
						_screen.setLastHasInputTime(new Date().getTime());
					}
				} else {
					// 入力欄はある画面だが、画面書き換えのタイミングでまだ入力欄が存在しないだけの場合は入力をバッファに戻して再実行
					_keyBuffer.unshift(e);
				}

				break;
			}

			if (_screen.getLastHasInputTime()) {
				// この時点では入力欄が存在するので時間を更新しておく。
				_screen.setLastHasInputTime(new Date().getTime());
			}

			// 以降は入力欄が存在するときだけ実行される処理

			// F11～F12は入力には使わない
			if (e.key == 'F11' || e.key == 'F12') {
				break;
			}

			let target = $targets[0];

			// 通常の文字入力か否か
			if (isKeyInputValue(e)) {
				// 文字挿入
				if (insertKey(e, target)) {
					break;
				}

				// 末尾に追加
				appendKey(e, target);

				screen.changeInput();

				break;
			}

			// DELETE
			if (e.key == 'Delete') {
				// DELETEキーはバックスペースキーと同じ操作を行う
				backspaceKey(target);
				break;
			}

			// カーソル移動
			if (e.key == 'ArrowRight') {
				arrowRightKey(target);
				break;
			}
			if (e.key == 'ArrowLeft') {
				arrowLeftKey(target);
				break;
			}

			// サーバーに送信するステータス値を取得する
			let statusValue = getStatusValue(e);

			// サーバーに送信するキーじゃない場合
			if (statusValue == null) {
				break;
			}

			let inputValue = '';
			// バックスペース、エスケープは入力内容を送信しないのでチェックしない
			if (e.key != 'Backspace' && e.key != 'Escape') {
				// 入力項目のチェックを行う
				let isTruncate = isTruncateValueKey(e);
				if (!elementInpCheck(target, isTruncate)) {
					break;
				}
				// 入力フォーマット（少数対応）
				inputValue = elementInpFormat(target, isTruncate);
			}

			ajaxSendParam({
				data : getKeyParams(inputValue, statusValue),
				success : function(msg) {
					if (e.key == 'Enter' || e.key == 'Escape') {
						// ブザーを停止
						_buzzer.stop();
					}
					_screen.screenReplace(msg, true);
				}
			});

			break;
		}

		// 再帰実行
		setTimeout(_that.keyBufferSimulate, KEY_BUFFER_SIMULATE_INTERVAL);
	};

	/**
	 * キーバッファリングした入力内容のうちサーバーに送信できるキーをまとめて送信する。
	 */
	let sendBatchParam = function(inputTarget) {
		let sendKeyParam = new Array();
		let isEnterKey = false;
		let isEscKey = false;
		// 入力値あり
		let isInput = 0 < inputTarget.value.length;

		// 入力チェックが不要なキーのみパラメータにセットして送る
		for (let i = 0; i < _keyBuffer.length; i++) {
			let e = _keyBuffer[i];

			// CTL+ANYキーの処理(画面切離関係)
			if (isScreenSwitch(e)) {
				break;
			}

			// CTL+ANYキーの処理
			if (isFuncSpecialKey(e)) {
				break;
			}

			// CTL+Functionキーの処理
			if (isKeyFunction(e)) {
				break;
			}

			// F11～F12は入力には使わないのでスキップ
			if (e.key == 'F11' || e.key == 'F12') {
				_keyBuffer.splice(i, 1);
				i--;
				continue;
			}

			// 通常の入力文字か否か
			if (isKeyInputValue(e)) {
				break;
			}

			// DELETE
			if (e.key == 'Delete') {
				break;
			}

			// カーソル移動
			if (e.key == 'ArrowRight') {
				break;
			}
			if (e.key == 'ArrowLeft') {
				break;
			}

			// サーバーに送信するステータス値を取得する
			let statusValue = getStatusValue(e);

			// 文字以外のサーバー送信不要なキーの場合は何もしないということなのでスキップする
			if (statusValue == null) {
				_keyBuffer.splice(i, 1);
				i--;
				continue;
			}

			let inputValue = '';

			// 入力チェックが不要なキーのみパラメータにセット
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

		// 送信
		if (0 < sendKeyParam.length) {
			ajaxSendParam({
				data : getKeyBatchParams(sendKeyParam),
				success : function(msg) {
					if (isEscKey || isEnterKey) {
						// ブザーを停止
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
	 * キーバッファリングをクリア
	 */
	let keyBufferClear = function() {
		_keyBuffer.length = 0;
	}

	/**
	 * 入力されたキーが入力文字か否か
	 */
	let isKeyInputValue = function(e) {
		// CTL or ALTが押されている
		if (e.ctrlKey == true || e.altKey == true) {
			return false;
		}

		// キーが2文字以上で表される
		if (1 < e.key.length) {
			return false;
		}

		let code = e.key.charCodeAt(0);
		// ASCIIコードのスペース～~までの範囲(制御コードを除く文字)
		if (32 <= code && code <= 126) {
			return true;
		}

		return false;
	}

	/**
	 * 入力されたキーがFunctionキーか否か
	 */
	let isKeyFunction = function(e) {
		// CTL + F1～F12
		if (e.ctrlKey == true) {
			if (e.key.match(/F[0-9]{1,2}/)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * 文字を挿入する。 カーソル位置が末尾の場合は制御不要なので何もしない。(falseを返却)
	 */
	let insertKey = function(e, field) {
		let startPos = field.selectionStart;
		let fieldValue = field.value;

		// 末尾にカーソルがある場合は追記になるので何もしない
		if (fieldValue.length <= startPos) {
			return false;
		}

		// maxLengthを超える部分は切り取る
		let insertValue = e.key;
		let newValue = fieldValue.slice(0, startPos) + insertValue
				+ fieldValue.slice(startPos + insertValue.length);
		if (field.maxLength < newValue.length) {
			newValue = newValue.slice(0, field.maxLength);
		}

		// 新しい文字を設定してカーソル位置を再設定する
		field.value = newValue;
		field.selectionStart = startPos + insertValue.length;
		field.selectionEnd = field.selectionStart;

		return true;
	}

	/**
	 * 文字を末尾に追加する。
	 */
	let appendKey = function(e, field) {
		let fieldValue = field.value;

		// maxLengthを超える場合は何もしない
		if (field.maxLength <= fieldValue.length) {
			return false;
		}
		let newValue = fieldValue + e.key;

		// 新しい文字を設定してカーソル位置を再設定する
		field.value = newValue;
		field.selectionStart = newValue.length;
		field.selectionEnd = field.selectionStart;

		return true;
	}

	/**
	 * バックスペースキーの動作を行う。
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
	 * 右矢印キーの動作を行う。
	 */
	let arrowRightKey = function(field) {
		if (field.selectionStart < field.value.length) {
			field.selectionStart = field.selectionStart + 1;
			field.selectionEnd = field.selectionStart;
		}
	}

	/**
	 * 左矢印キーの動作を行う。
	 */
	let arrowLeftKey = function(field) {
		if (0 < field.selectionStart) {
			field.selectionStart = field.selectionStart - 1;
			field.selectionEnd = field.selectionStart;
		}
	}

	/**
	 * サーバーに送信するステータスの値を取得する。 サーバーに送信するキーでない場合はnullを返す。
	 */
	let getStatusValue = function(e) {

		let statusValue = null;

		// 入力値の設定
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
			// 2バイト入力
			// Firefoxでは発生しない
			statusValue = null;
		default:
			statusValue = null;
		}

		return statusValue;
	}

	/**
	 * 入力された文字をカーソル位置で切り捨てるキーか否かを判断する。
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
	 * キー入力のパラメータを取得する。
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
	 * キー入力のパラメータを取得する。(バッチ用)
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
 * ブザー制御を行う
 */
var BuzzerControl = function() {
	let _buzzerCounter = -1;
	let _buzzerTimeoutId = -1;

	let buzzer = function(soundLength) {
		if (-1 < _buzzerTimeoutId) {
			clearTimeout(_buzzerTimeoutId);
		}

		if (_buzzerCounter++ < soundLength) {
			// base64文字列を貼り付け(音の長さをここでコントロールする必要有り？
			let base64 = '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'
			// datauri scheme 形式にして Audio オブジェクトを生成します
			let sound = new Audio('data:audio/wav;base64,' + base64);
			// 音を鳴らします
			sound.play();
			/*
			 * var msg = 'エラーが発生しました。\n'; msg += '管理者に連絡してください。\n'; msg +=
			 * soundLength + ''; alert(msg); BuzzerFlg = 1;
			 */

			// 次のタイマーをセット
			_buzzerTimeoutId = setTimeout(function() {
				buzzer(soundLength);
			}, BUZZER_INTERVAL);
		}
	};

	/**
	 * ブザーを止める
	 */
	this.stop = function() {
		if (this.isBuzzer()) {
			clearTimeout(_buzzerTimeoutId);
			_buzzerCounter = -1;
		}
	};

	/**
	 * ブザーを鳴らす
	 */
	this.start = function(soundLength) {
		// 既に再生している場合は上書き再生はしない
		if (!this.isBuzzer()) {
			_buzzerCounter = 0;
			buzzer(soundLength);
		}
	};

	/**
	 * ブザーを鳴らしているか否か
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
