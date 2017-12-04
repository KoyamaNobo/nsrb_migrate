var procStatusUpdate = function() {
	let esStatus;

	// 初期処理
	init();

	// SSE開始
	startStatusUpdateListener();

	/**
	 * 画面遷移前にSSEの接続を切断する。
	 */
	$(window).on("beforeunload", function() {
		if (esStatus) {
			esStatus.close();
			esStatus = null;
		}
	});

	/**
	 * 初期処理を行う。 (SSEのイベントリスナーが開始されるまでに1、2秒程度のタイムラグがあるので初めはAjaxで値を取得する。)
	 */
	function init() {
		let queryParams = {
			pid : $('#pid').val()
		};

		$.ajax({
			type : "GET",
			url : "proc_status.php",
			data : queryParams,
			success : function(msg, txt) {
				onmessage(msg);
			},
			error : function(jqXHR, textStatus, errorThrown) {
				// イベントリスナーのほうで再処理が行わるのでここでは何もしない。
				if (errorThrown) {
					console.log("Status connect Error" + ":"
							+ errorThrown.message);
				}
			}
		});
	}

	/**
	 * ステータスを更新するSSEを開始する。
	 */
	function startStatusUpdateListener() {
		let queryParams = {
			pid : $('#pid').val()
		};
		let url = buildUrl('proc_status.php', queryParams);

		esStatus = new EventSource(url);
		esStatus.onmessage = function(e) {
			// メッセージ受信
			onmessage(e.data);
		};
	}

	/**
	 * 受信したメッセージを処理する。
	 */
	function onmessage(data) {
		let $changeObj = $('<div>');
		$changeObj.html(data);

		// プロセスが終了しているか否か
		let procesEnd = $changeObj.find('#procesEnd');
		if (0 < procesEnd.length) {
			// プロセスが終了している場合は接続を切断して処理を終了する。
			if (esStatus) {
				esStatus.close();
				esStatus = null;
			}
			return;
		}

		// changeObjの中からstatus2Getを消す
		let status2 = $changeObj.find('#status2');
		if (0 < status2.length) {
			$("#status2").html("<span>" + status2[0].value + "</span>");
			status2.remove();
		}
		// changeObjの中からstatus4Getを消す
		let status4 = $changeObj.find('#status4');
		if (0 < status4.length) {
			$("#status4").html("<span>" + status4[0].value + "</span>");
			status4.remove();
		}
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

addEvent('load', window, procStatusUpdate);