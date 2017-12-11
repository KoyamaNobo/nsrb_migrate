/**
 * フォーカス設定を行う間隔(msec)
 */
var SET_FOCUS_INTERVAL = 100;
/**
 * Beep音を鳴らす間隔(msec)
 */
var BEEP_INTERVAL = 600;
/**
 * セッション継続用ポーリング間隔(msec)
 */
var SESSION_ALIVE_INTERVAL = 1000 * 60 * 5;

/**
 * 処理中を表すキュー。 キューにデータが存在する場合はキー入力をブロックしてバッファリングする。
 * ただし、日本語入力をブロックすることはできないので、ローマ字入力と日本語入力とでは 動作が異なることになる。
 */
var processQueue = new Array();

/**
 * キー入力をバッファリングしたリスト
 */
var keyBuffer = new Array();

/**
 * history.backを1度以上しないため
 */
var historyBackFlg = false;

/**
 * 現在発生中のエラー
 */
var currentErrors = new Array();

/**
 * 画面データのタイムスタンプ。 古いデータをスキップするための判定用
 */
var dataTimestamp = 0;

/**
 * STOP中か否か 何かキーが入力されるまで入力を受け付けない
 */
var isStop = false;

/**
 * Beep音を指定回数鳴らすために使うカウン。 マイナス値の場合はBeep音は停止中を表す。
 */
var beepCounter = -1;

/**
 * Beep音を鳴らすためのタイムアウト関数のID
 */
var beepTimeoutId;

/**
 * ブラウザがFirefoxか否か
 */
var isFirefox = (function() {
	var userAgent = window.navigator.userAgent.toLowerCase();
	return userAgent.indexOf('firefox') != -1;

})();

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

	keyBufferSimulate();
}

/**
 * メニュー画面か否か
 */
var isMenuScreen = function() {
	var infname = $('#infname');
	// menu等の場合infnameがない
	if (infname.length == 0) {
		return true;
	}

	return false;
}

/**
 * 前画面に戻る。
 */
var historyBack = function() {
	if (!historyBackFlg) {
		history.back();
		historyBackFlg = true;
	}
}

/**
 * プロセスが終了しているか否か
 */
var isProcessEnd = function() {
	$parentStatus = $('#parentStatus');

	if (0 < $parentStatus.length) {
		if ($parentStatus.val() == 'end') {
			// プロセス終了
			return true;
		}
	}

	return false;
}

/**
 * エラーが発生しているか否か
 */
var hasError = function() {
	let html = $('#status1').html();
	return 0 < $.trim(html.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g, '')).length;
}

/**
 * 画面の要素を入れ替える。
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

	// 取得行のいれかえ
	replaceScreenElement($tarScreen, $chgScreen);
}

/**
 * 画面データのタイムスタンプをチェックする。
 */
var checkTimestamp = function($chgScreen) {
	$dataTimestamp = $chgScreen.find('#dataTimestamp');
	newDataTimestamp = parseFloat($dataTimestamp.val());

	// 読み取り後に要素は削除
	$dataTimestamp.remove();

	if (dataTimestamp < newDataTimestamp) {
		// データが新しくなっている場合
		dataTimestamp = newDataTimestamp;
		return true;
	}

	return false;
}

/**
 * 処理のステータスに合わせてステータスを更新＆処理を終了する。
 */
var checkStatus = function($chgScreen) {
	// $chgScreenの中からerror探して表示
	$errors = $chgScreen.find('.error');
	if (0 < $errors.length) {
		// エラーあり
		var errorValue = $errors[0].value;
		var $status1 = $('#status1');
		$status1.html('<span>' + errorValue + '</span>');
	}

	// parentStatusGetが存在している場合parentStatusに代入して
	// $chgScreenの中からparentStatusGetを消す
	let $parentStatusGet = $chgScreen.find('.parentStatusGet');
	if (0 < $parentStatusGet.length) {
		$('#parentStatus').val($parentStatusGet.val());
		$parentStatusGet.remove();
	}

	// プロセスが終了した場合は画面更新のSSEを停止
	if (isProcessEnd()) {
		stopScreenUpdateListener();
	}

	// エラーが発生していなくてプロセスが終了している場合は前画面に戻る。(画面を終了する)
	if (isProcessEnd() && !hasError()) {
		historyBack();
		return false;
	}

	return true;
}

/**
 * 画面内容を更新する
 */
var replaceScreenElement = function($tarScreen, $chgScreen) {
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
	}

	// 要素の入れ替え方法を決定
	// 現在と次の画面に同じ入力項目がある場合のみ行単位で要素の入れ替えを行う。
	let isLineChange = false;
	if (0 < $tarInputs.length && 0 < $chgInputs.length) {
		let tarInput = $tarInputs[0];
		let chgInput = $chgInputs[0];
		if (tarInput.name == chgInput.name) {
			var tarColumns = tarInput.className.match(/\s+f[0-9]+/i);
			var chgColumns = chgInput.className.match(/\s+f[0-9]+/i);

			if (tarColumns[0] == chgColumns[0]) {
				// nameが同じでかつクラスのフィールド名が同じ
				isLineChange = true;
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
		$tarLines = $tarScreen.children();
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

			// 入力要素のある行は入力要素以外を削除して入れ替えを行う。
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
		stopStop();
	} else if (isChgStop) {
		// STOPが現れた
		startStop();
	}

	// モーダル
	$errBuz = $chgScreen.find('#err-buz');
	if (0 < $errBuz.length) {
		startBeep(parseInt($errBuz.val()));
	}
	// 音のみ
	$infoBuz = $chgScreen.find('#info-buz');
	if (0 < $infoBuz.length) {
		startBeep(parseInt($infoBuz.val()));
	}

	// フォーカスを合わせる
	setNextInputFocus();
}

/**
 * 点滅表示設定を行う処理。
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
 * 定期的に画面内容を更新する処理。
 */
let esScreen;
var screenUpdate = function() {
	let setFocusTimeoutId;

	if (!isMenuScreen()) {
		// PIDが取得できない場合は前画面に戻り処理は何も行わない。
		if ($('#pid').length == 0) {
			historyBack();
			return;
		}

		// 初期処理
		init();

		// SSE開始
		startScreenUpdateListener();

		// 入力欄にフォーカスを合わせる処理を開始
		setFocusTimeoutId = setTimeout(setInputFocus, SET_FOCUS_INTERVAL);

		/**
		 * 画面遷移前にSSEの接続を切断する。
		 */
		$(window).on('beforeunload', function() {
			stopScreenUpdateListener();

			// フォーカスタイマーも停止
			clearTimeout(setFocusTimeoutId);
		});
	}

	/**
	 * 初期処理を行う。 (SSEのイベントリスナーが開始されるまでに1、2秒程度のタイムラグがあるので初めはAjaxで値を取得する。)
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
				// イベントリスナーのほうで再処理が行わるのでここでは何もしない。
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
	 * セッションタイムアウト対策として定期的にリクエストを行う。
	 */
	function polling() {

		// 通信結果は無視する
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
	 * 画面内容を更新するSSEを開始する。
	 */
	function startScreenUpdateListener() {
		let url = buildUrl('getOut.php', getQueryParams());

		esScreen = new EventSource(url);
		esScreen.onmessage = function(e) {
			// メッセージ受信
			preProcess();

			try {
				onmessage(e.data);
			} finally {
				postProcess();
			}
		};
	}

	/**
	 * パラメータを取得する。
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
	 * 入力欄にフォーカスを合わせる。
	 */
	function setInputFocus() {
		clearTimeout(setFocusTimeoutId);

		setNextInputFocus();

		setFocusTimeoutId = setTimeout(setInputFocus, SET_FOCUS_INTERVAL);
	}

	/**
	 * 受信したメッセージを処理する。
	 */
	function onmessage(data) {
		// 画面内容の更新
		screenReplace(data);
	}

	/**
	 * URLを組み立てる
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
 * 入力欄にフォーカスを合わせる。
 */
function setNextInputFocus() {
	if (document.activeElement.id != 'skSelect') {
		$('input.nextinput:last').focus();
	}
}

/**
 * 画面内容を更新するSSEを停止する。
 */
function stopScreenUpdateListener() {
	if (esScreen) {
		esScreen.close();
		esScreen = null;
	}
}

/**
 * バッファリングしたキー入力を実行する
 *
 */
function keyBufferSimulate() {
	// エラーメッセージが表示中 かつプロセス終了時はキー操作無効なためバッファクリア
	if (isProcessEnd() && hasError()) {
		keyBufferClear();
		return;
	}

	// 処理中の場合は何もせずに終了
	if (0 < processQueue.length) {
		return;
	}

	// バッファに何もない場合は何もしない。
	if (keyBuffer.length == 0) {
		return;
	}

	let $targets = $('input.nextinput');

	if (0 < $targets.length) {
		// まとめて送れるものは送る
		if (sendBatchParam($targets[0])) {
			return;
		}
	}

	for (var i = 0; i < 1; i++) {
		e = keyBuffer.shift();

		// CTL+ANYキーの処理(画面切離関係)
		if (screenSwitch(e)) {
			break;
		}

		// CTL+ANYキーの処理
		if (funcSpecialKey(e)) {
			break;
		}

		if ($targets.length == 0) {
			// FIXME 画面が切り替わる前なので項目がない場合がある。なので、入力項目が現れるまで再実行する必要あり。
			// もしくは入力欄関係なくPOSTしていく？その場合は属性チェックとMAXLENGTHは無視することになる。
			// FIXME 一旦再処理させる
			keyBuffer.unshift(e);
			if (true)
				break;
			//

			console.log('no input');
			// 入力欄が存在しない場合
			// F1～F12のキー (FIXME この処理必要？)
			if (e.key.match(/F[0-9]{1,2}/)) {
				if (e.ctrlKey == true) {
					sendFunctionKey(e);
				}
			}
			break;
		}

		// F11～F12は使用禁止
		if (e.key == 'F11' || e.key == 'F12') {
			break;
		}

		let target = $targets[0];

		let inputValue = '';
		let isInputCheck = true;

		// Backspace、Escapeの場合は入力値は不要なのでチェックしない。
		if (e.key != 'Backspace' && e.key != 'Escape') {
			// 入力チェック
			if (elementInpCheck(target)) {
				// 入力フォーマット（少数対応）
				inputValue = elementInpFormat(target);
				isInputCheck = true;
			} else {
				isInputCheck = false;
			}
		}

		// 通常の入力文字か否か
		if (isInputCharValue(e)) {
			if (insertCharValue(e, target)) {
				break;
			}

			// 末尾に追加
			appendCharValue(e, target);

			// FIXME 背景色設定 一旦このタイミングで。
			setInputColor();

			break;
		}

		// DELETE
		if (e.key == 'Delete') {
			delKey(target);
			break;
		}

		let sendParams = getSendParam(e, inputValue);

		// サーバー送信不要なキーの場合はデータをサーバーに送信しない
		if (sendParams[0] == false) {
			break;
		}

		// 入力チェックNGの場合はデータをサーバーに送信しない
		if (!isInputCheck) {
			break;
		}

		ajaxSendParam({
			data : getKeyParams(sendParams[1], sendParams[2]),
			success : function(msg) {
				if (e.key == 'Enter') {
					// Beep音を停止
					stopBeep();

					screenReplace(msg);
				} else if (e.key == 'Escape') {
					// Beep音を停止
					stopBeep();

					if (isProcessEnd() && hasError()) {
						// プロセスが終了してエラーが表示されている場合は前画面へ
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

	// 再帰実行
	setTimeout(keyBufferSimulate, 100);
}

/**
 * キーバッファリングした入力内容のうちサーバーに送信できるキーをまとめて送信する。
 */
function sendBatchParam(inputTarget) {
	let sendKeyParam = new Array();
	let isEnterKey = false;
	let isEscKey = false;
	// 入力値あり
	let isInput = 0 < inputTarget.value.length;

	// 入力チェックが不要なキーのみパラメータにセットして送る
	for (var i = 0; i < keyBuffer.length; i++) {
		let e = keyBuffer[i];

		// CTL+ANYキーの処理(画面切離関係)
		if (isScreenSwitch(e)) {
			break;
		}

		// CTL+ANYキーの処理
		if (isFuncSpecialKey(e)) {
			break;
		}

		// F11～F12は使用禁止なのでスキップ
		if (e.key == 'F11' || e.key == 'F12') {
			keyBuffer.splice(i, 1);
			i--;
			continue;
		}

		// 通常の入力文字か否か
		if (isInputCharValue(e)) {
			break;
		}

		// DELETE
		if (e.key == 'Delete') {
			break;
		}

		let sendParams = getSendParam(e, '');

		// 文字以外のサーバー送信不要なキーの場合は何もしないということなのでスキップする。
		if (sendParams[0] == false) {
			keyBuffer.splice(i, 1);
			i--;
			continue;
		}

		// 入力チェックが不要なキーのみパラメータにセット
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

	// 送信
	if (0 < sendKeyParam.length) {
		ajaxSendParam({
			data : getKeyBatchParams(sendKeyParam),
			success : function(msg) {
				if (isEscKey) {
					// Beep音を停止
					stopBeep();

					if (isProcessEnd() && hasError()) {
						// プロセスが終了してエラーが表示されている場合は前画面へ
						historyBack();
					} else {
						screenReplace(msg);
					}
				} else if (isEnterKey) {
					// Beep音を停止
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
 * キーバッファリングをクリア
 */
function keyBufferClear() {
	keyBuffer.length = 0;
}

/**
 * キー入力制御
 */
var keyControl = function() {
	// Firefoxの日本語入力確定判定のためのフラグ
	// Firefoxの場合は日本語入力のキーイベントは日本語確定時のエンターキーがkeyup時にのみに発生する。
	// そこから日本語入力されたか否かを判定する。
	let isKeydown = false;

	$(document).keydown(
			function(e) {
				console.log('keydown:' + e.key + ':' + e.keyCode);

				// エラーメッセージが表示中 かつプロセス終了時はキー操作無効
				if (isProcessEnd() && hasError()) {
					return false;
				}

				if (isMenuScreen()) {
					// メニュー画面

					// CTL+ANYキーの処理(画面切離関係)
					if (screenSwitch(e)) {
						return false;
					}

					// F1～F12、ALT、ESCは使用禁止
					if (e.key.match(/F[0-9]{1,2}/) || e.key == 'Alt'
							|| e.key == 'Escape') {
						return false;
					}
				} else {
					// メニュー以外の画面

					// 日本語入力判定用
					if (isFirefox && e.key == 'Enter') {
						isKeydown = true;
					}

					if (0 < processQueue.length) {
						// 処理中
						// キー入力をバッファリングする。
						keyBuffer.push(e);
						console.log("buffer push:" + e.key + ":" + e.keyCode);

						return false;
					} else {
						if (0 < keyBuffer.length) {
							// バッファリングしたキー入力が残っている場合は入力されたキーをバッファの末尾に追加し、キー実行
							keyBuffer.push(e);
							keyBufferSimulate();

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

						let $targets = $('input.nextinput');

						if ($targets.length == 0) {
							// 入力欄が存在しない場合
							// FIXME 入力項目が存在しない場合はバッファに追加していったん終了
							// FIXME 入力項目がない画面もある？
							keyBuffer.push(e);
							if (true)
								return false;
							//

							console.log('no input hon');
							// F1～F12のキー (FIXME この処理必要？)
							if (e.key.match(/F[0-9]{1,2}/)) {
								if (e.ctrlKey == true) {
									sendFunctionKey(e);
								}
							}
							return false;
						}

						// F11～F12は使用禁止
						if (e.key == 'F11' || e.key == 'F12') {
							return false;
						}

						let target = $targets[0];

						let inputValue = '';
						let isInputCheck = true;

						// Backspaceの場合は入力値は不要なのでチェックしない。
						if (e.key != 'Backspace' && e.key != 'Escape') {
							// 入力チェック
							// FIXME
							// 入力チェックのタイミングがおかしい。これだとエラーが表示されるタイミングがおかしくなる。
							if (elementInpCheck(target)) {
								// 入力フォーマット（少数対応）
								inputValue = elementInpFormat(target);
								isInputCheck = true;
							} else {
								isInputCheck = false;
							}
						}

						// 通常の入力文字か否か
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

						// サーバー送信不要なキーの場合は入力チェックだけして文字の入力は許可
						if (sendParams[0] == false) {
							return true;
						}

						// 入力チェックNGの場合はデータをサーバーに送信しない
						if (!isInputCheck) {
							return false;
						}

						ajaxSendParam({
							data : getKeyParams(sendParams[1], sendParams[2]),
							success : function(msg) {
								if (e.key == 'Enter') {
									// Beep音を停止
									stopBeep();

									screenReplace(msg);
								} else if (e.key == 'Escape') {
									// Beep音を停止
									stopBeep();

									if (isProcessEnd() && hasError()) {
										// プロセスが終了してエラーが表示されている場合は前画面へ
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
		 * キーアップイベント
		 */
		$(document).keyup(function(e) {
			// console.log('keyup:' + e.key + ':' + e.keyCode);
			let isProcess = false;

			if (isFirefox && e.key == 'Enter') {
				if (!isKeydown) {
					// 日本語入力確定のエンター
					isProcess = true;
				}
				isKeydown = false;
			}

			// 日本語入力がされた場合は入力順序が逆転してしまうのでバッファはクリア
			if (isProcess) {
				keyBufferClear();
			}

			// 背景色設定
			setInputColor();

			return true;
		});
	}
};

/**
 * 入力文字か否か判定する
 */
var isInputCharValue = function(e) {
	// CTL or ALTが押されている
	if (e.ctrlKey == true || e.altKey == true) {
		return false;
	}

	if (1 < e.key.length) {
		// キーが2文字以上で表される場合
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
 * 文字を挿入する。 カーソル位置が末尾の場合は制御不要なので何もしない。(falseを返却)
 */
var insertCharValue = function(e, field) {
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
	field.selectionEnd = startPos + insertValue.length;

	return true;
}

/**
 * 文字を末尾に追加する。
 */
var appendCharValue = function(e, field) {
	let fieldValue = field.value;

	// maxLengthを超える場合は何もしない。
	if (field.maxLength <= fieldValue.length) {
		return false;
	}
	let newValue = fieldValue + e.key;

	// 新しい文字を設定してカーソル位置を再設定する
	field.value = newValue;
	field.selectionStart = newValue.length;
	field.selectionEnd = newValue.length;

	return true;
}

/**
 * DELETEキー
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
 * サーバーに送信するパラメータを取得する。
 */
var getSendParam = function(e, inputValue) {

	let statusValue = '';

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
		// 2バイト入力
		// Firefoxでは発生しない。
		return [ false, '', '' ];
	default:
		return [ false, '', '' ];
	}

	return [ true, inputValue, statusValue ];
}

/**
 * 背景色を設定する
 */
var setInputColor = function() {
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
	}
}

/**
 * F1～F12のコードを送信する。
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
 * キー入力のパラメータを取得する。
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
 * キー入力のパラメータを取得する。(バッチ用)
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
 * STOP開始
 */
var startStop = function() {
	isStop = true;
}

/**
 * STOP停止
 */
var stopStop = function() {
	isStop = false;
}

/**
 * STOP中か否か
 */
var isStoping = function() {
	return isStop;
}

/**
 * Beep音の再生を開始する
 */
var startBeep = function(soundLength) {
	// 既に再生している場合は上書き再生はしない
	if (!isBeeping()) {
		beepCounter = 0;
		beep(soundLength);
	}
}

/**
 * Beep音を止める
 */
var stopBeep = function() {
	if (isBeeping()) {
		clearTimeout(beepTimeoutId);
		beepCounter = -1;
	}
}

/**
 * Beep音が再生中か否か
 */
var isBeeping = function() {
	return -1 < beepCounter;
}

/**
 * Beep音を再生する
 */
var beep = function(soundLength) {
	clearTimeout(beepTimeoutId);

	if (beepCounter++ < soundLength) {
		// base64文字列を貼り付け(音の長さをここでコントロールする必要有り？
		var base64 = 'UklGRnoGAABXQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQoGAACBhYqFbF1fdJivrJBhNjVgodDbq2EcBj+a2/LDciUFLIHO8tiJNwgZaLvt559NEAxQp+PwtmMcBjiR1/LMeSwFJHfH8N2QQAoUXrTp66hVFApGn+DyvmwhBTGH0fPTgjMGHm7A7+OZSA0PVqzn77BdGAg+ltryxnMpBSl+zPLaizsIGGS57OihUBELTKXh8bllHgU2jdXzzn0vBSF1xe/glEILElyx6OyrWBUIQ5zd8sFuJAUuhM/z1YU2Bhxqvu7mnEoODlOq5O+zYBoGPJPY88p2KwUme8rx3I4+CRZiturqpVITC0mi4PK8aB8GM4nU8tGAMQYfcsLu45ZFDBFYr+ftrVoXCECY3PLEcSYELIHO8diJOQcZaLvt559NEAxPqOPwtmMcBjiP1/PMeS0GI3fH8N2RQAoUXrTp66hVFApGnt/yvmwhBTCG0fPTgjQGHW/A7eSaRw0PVqzl77BeGQc9ltvyxnUoBSh+zPDaizsIGGS56+mjTxELTKXh8bllHgU1jdT0z3wvBSJ0xe/glEILElyx6OyrWRUIRJve8sFuJAUug8/y1oU2Bhxqvu3mnEoPDlOq5O+zYRsGPJLZ88p3KgUme8rx3I4+CRVht+rqpVMSC0mh4fK8aiAFM4nU8tGAMQYfccPu45ZFDBFYr+ftrVwWCECY3PLEcSYGK4DN8tiIOQcZZ7zs56BODwxPpuPxtmQcBjiP1/PMeywGI3fH8N+RQAoUXrTp66hWEwlGnt/yv2wiBDCG0fPTgzQHHG/A7eSaSQ0PVqvm77BeGQc9ltrzxnUoBSh9y/HajDsIF2W56+mjUREKTKPi8blnHgU1jdTy0HwvBSF0xPDglEQKElux6eyrWRUJQ5vd88FwJAQug8/y1oY2Bhxqvu3mnEwODVKp5e+zYRsGOpPX88p3KgUmecnw3Y4/CBVhtuvqpVMSC0mh4PG9aiAFM4nS89GAMQYfccLv45dGCxFYrufur1sYB0CY3PLEcycFKoDN8tiIOQcZZ7rs56BODwxPpuPxtmQdBTiP1/PMey4FI3bH8d+RQQkUXbPq66hWFQlGnt/yv2wiBDCG0PPTgzUGHG3A7uSaSQ0PVKzm7rJeGAc9ltrzyHQpBSh9y/HajDwIF2S46+mjUREKTKPi8blnHwU1jdTy0H4wBiF0xPDglEQKElux5+2sWBUJQ5vd88NvJAUtg87y1oY3Bxtpve3mnUsODlKp5PC1YRsHOpHY88p3LAUlecnw3Y8+CBZhtuvqpVMSC0mh4PG9aiAFMojT89GBMgUfccLv45dGDRBYrufur1sYB0CX2/PEcycFKoDN8tiKOQgZZ7vs56BOEQxPpuPxt2MdBTeP1vTNei4FI3bH79+RQQsUXbTo7KlXFAlFnd7zv2wiBDCF0fLUgzUGHG3A7uSaSQ0PVKzm7rJfGQc9lNrzyHUpBCh9y/HajDwJFmS46+mjUhEKTKLh8btmHwU1i9Xyz34wBiFzxfDglUMMEVux5+2sWhYIQprd88NvJAUsgs/y1oY3Bxpqve3mnUsODlKp5PC1YhsGOpHY88p5KwUlecnw3Y8+ChVgtunqp1QTCkig4PG9ayEEMojT89GBMgUfb8Lv4pdGDRBXr+fur1wXB0CX2/PEcycFKn/M8diKOQgZZrvs56BPEAxOpePxt2UcBzaP1vLOfC0FJHbH79+RQQsUXbTo7KlXFAlFnd7xwG4jBS+F0fLUhDQGHG3A7uSbSg0PVKrl7rJfGQc9lNn0yHUpBCh7yvLajTsJFmS46umkUREMSqPh8btoHgY0i9Tz0H4wBiFzw+/hlUULEVqw6O2sWhYIQprc88NxJQUsgs/y1oY3BxpqvO7mnUwPDVKo5PC1YhsGOpHY8sp5KwUleMjx3Y9ACRVgterqp1QTCkig3/K+aiEGMYjS89GBMgceb8Hu45lHDBBXrebvr1wYBz+Y2/PGcigEKn/M8dqJOwgZZrrs6KFOEAxOpd/js2coGUCLydq6e0MlP3uwybiNWDhEa5yztJRrS0lnjKOkk3leWGeAlZePfHRpbH2JhoJ+fXl9TElTVEQAAABJTkZPSUNSRAsAAAAyMDAxLTAxLTIzAABJRU5HCwAAAFRlZCBCcm9va3MAAElTRlQQAAAAU291bmQgRm9yZ2UgNC41AA=='
		// datauri scheme 形式にして Audio オブジェクトを生成します
		var sound = new Audio('data:audio/wav;base64,' + base64);
		// 音を鳴らします
		sound.play();
		/*
		 * var msg = 'エラーが発生しました。\n'; msg += '管理者に連絡してください。\n'; msg +=
		 * soundLength + ''; alert(msg); BuzzerFlg = 1;
		 */

		// 次のタイマーをセット
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
// // base64文字列を貼り付け(音の長さをここでコントロールする必要有り？
// var base64 =
// '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'
// // datauri scheme 形式にして Audio オブジェクトを生成します
// var sound = new Audio('data:audio/wav;base64,' + base64);
// // 音を鳴らします
// sound.play();
// /*
// * var msg = 'エラーが発生しました。\n'; msg += '管理者に連絡してください。\n'; msg +=
// * soundLength + ''; alert(msg); BuzzerFlg = 1;
// */
//
// // 次のタイマーをセット
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
// // 既に再生している場合は上書き再生はしない
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
 * パラメータ送信用のAjax拡張
 */
var ajaxSendParam = function(arg) {
	var opt = $.extend({}, $.ajaxSettings, arg);

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
		return function(data, statusText, jqXHR) {
			if (func) {
				func(data, statusText, jqXHR);
			}
		};
	})(opt.success);

	// エラー
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

	// 完了
	opt.complete = (function(func) {
		return function(jqXHR, statusText) {
			if (func) {
				func(jqXHR, statusText);
			}

			// 後処理
			postProcess();
		};
	})(opt.complete);

	return $.ajax(opt);
};

addEvent('load', window, screenUpdate);
addEvent('load', window, keyControl);
