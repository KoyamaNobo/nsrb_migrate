//setTimeout用の変数
var TimeoutID;
//buzzer用の変数(ならしたら立てる)
var BuzzerFlg=0;
//buzzer用のtimeOutID
var BuzzerTimeoutID;
//一度文字入力したら帰ってくるまで送らない
var sendFlag = false;
//Error文字列の配列
var ErrorStrArray = new Array();
//エスケープボタンを押下フラグ（押下時true）
var escKeyFlg = false;
//タイムアウト時間 16以下はFireFoxは動かない
var TimeoutTime = 16;
//タイムアウト時間 16以下はFireFoxは動かない
var ReplaceModeFlg = 0;
//history.back()を一度以上しないため
var historyBackFlg = false;

//定期的に画面をとってくる
function fncChangeScreen() {
	var outfname = $('#outfname')[0];
	var infname  = $('#infname')[0];
	var pid      = $('#pid')[0];
	//menu等の場合infnameがない
	if( infname == void 0){
		return ;
	}
	if(sendFlag == false){
		$.ajax({
			type: "POST",
			url: "getOut.php",
			data:{ infname:infname.value, outfname:outfname.value, pid:pid.value },
			success: function(msg,txt){
				screenReplace(msg);
			},
			error: function(jqXHR,textStatus,errorThrown ){
				if(errorThrown){
					alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					reStart();
				}
			}
		});
		outfname = null;
		infname  = null;
		pid = null;
	}else{
		outfname = null;
		infname  = null;
		pid = null;
		reStart();
	}
};


//入力えんたー時パラメータを送信
function nextInput(event){
	//入力変換
	//author : koyama 20170202
	function change_input_send(event){
		let inputValue='';
		let statusValue='';
		//入力チェック
		if(inpCheck(event)){
			//入力フォーマット（少数対応）
			inputValue = inpFormat(event);
		}else{
			return false;
		}
		//入力値の設定
		switch (event.key) {
			case 'Enter' :
				if(event.shiftKey == true){
					statusValue = 'f';
					break;
				}
				statusValue = 'h';
				break;
			case 'Tab' :
				statusValue = 's';
				break;
			case 'Backspace' :
				if(targElem.value != ""){
					return true;
				}
				inputValue = '';
				statusValue = 'b';
				break;
			case 'F10':
			case 'Alt':
				if(event.shiftKey == true){
					statusValue = '10';
				}else{
					statusValue = 'a';
				}
				break;
			case 'F1' :
			case 'F2' :
			case 'F3' :
			case 'F4' :
			case 'F5' :
			case 'F6' :
			case 'F7' :
			case 'F8' :
			case 'F9' :
				if(event.key == 'F4' && event.shiftKey == true){
					statusValue = '14';
					break;
				}
				if(event.key == 'F8' && event.shiftKey == true){
					statusValue = 'c1';
					break;
				}
				if(event.key == 'F9' && event.shiftKey == true){
					statusValue = 'c2';
					break;
				}
				statusValue = event.key.substr(1);
				break;
				case 'Process' :
					//2バイト入力
					replaceModeExec(event);
					// 全角半角キーを押されたらとりあえず送信はしない
					return true;
			default:
				return true;
		}
		//sendFlagとTimeoutの処理
		preProcess();
		$.ajax({
			type: "POST",
			url: "param.php",
			data:{ value:inputValue + '`'+ statusValue ,infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
			success: function(msg){
				screenReplace( msg );
				sendFlag = false;
			},
			error: function(jqXHR,textStatus,errorThrown ){
				alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
				console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			}
		});
		//途中でtrueとして抜けない限りfalse
		//処理をここで止めるときはfalseを返す
		return false;
	}

	//上書きモードの時,後ろの一文字を消す
	function replaceModeExec(event){
		//event.target.selectionStart ->これがキャレットの位置
		event.target.value = event.target.value.substr(0,event.target.selectionStart);
	}
	let targElem = evtToElement(event);
	let outfname = $('#outfname');
	let infname  = $('#infname');
	let pid  = $('#pid');
	let input = '';

	//menu等の場合infnameがない
	if( infname == void 0){
		return ;
	}
	if(sendFlag == false){
		//エラーメッセージが表示中　かつ
		//ジョブ終了時はキー操作無効
		let tmp = $("#status1").html();
		tmp = jQuery.trim(tmp.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g,''));
		if(tmp.length > 0 && $('#parentStatus')[0].value == "end" ){
			return false;
		}
		//CTL+ANYキーの処理(画面切離関係)
		if(screenSwitch(event)){
			//特殊キーの場合はこれ以上処理しない
			event.stopImmediatePropagation();
			// return false;
		}

		//CTL+ANYキーの処理
		if(funcSpecialKey(event)){
			//特殊キーの場合はこれ以上処理しない
			event.stopImmediatePropagation();
			// return false;
		}
		//入力に何かあれば下記関数で、何もなければtrueで帰ってくる
		let retVal;
		retVal = change_input_send(event);
		if(retVal == false){
			//入力があったときはバブリングキャンセル
			// if(event.defaultPrevented != true){
			// 	event.preventDefault();
			// }
			// event.stopImmediatePropagation();
			// event.stopPropagation();
		}
		// return retVal;
	}
};

//フォーカスがinputではないときのキーイベント
var anyKeyEvent = function(evt){
	var parentStatus  = $('#parentStatus');

	//エラーメッセージが表示中　かつ
	//ジョブ終了時はキー操作無効
	let tmp = $("#status1").html();
	tmp = jQuery.trim(tmp.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g,''));
	if(tmp.length > 0  && parentStatus[0].value == "end" ){
		return false;
	}

	if(sendFlag != false){
		return true;
	}
	//F5 + ctrl
	if(evt.keyCode >= 112 && evt.keyCode <= 123){
		let targElem = evtToElement(evt);
		if(evt.ctrlKey == true){
			//これとinputじゃないときに動く？
			//CTL+ANYキーの処理(画面切離関係)
			if(screenSwitch(evt)){
				//特殊キーの場合はこれ以上処理しない
				return false;
			}

			let menuCmb = $('#root')[0];
			var forcusElem = (document.activeElement || window.getSelection().focusNode);
			if(forcusElem == menuCmb || forcusElem.tagName.toLowerCase() == 'input'){
				forcusElem=null;
				menuCmb   =null;
				return false;
			}

			//CTL+ANYキーの処理
			if(funcSpecialKey(evt)){
				//特殊キーの場合はこれ以上処理しない
				return false;
			}

			sendFlag = true;
			//menu等の場合infnameがない
			if($('#infname').length != 0){
				var outfname = $('#outfname');
				var infname  = $('#infname');
				var pid      = $('#pid');
				//sendFlagとTimeoutの処理
				preProcess();
				$.ajax({
					type: "POST",
					url: "param.php",
					data:{ value:'F' + (evt.keyCode - 111),infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
					success: function(msg){
						screenReplace( msg );
						sendFlag = false;
					},
					error: function(jqXHR,textStatus,errorThrown ){
						alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
						console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					}
				});
				outfname=null;
				infname =null;
				pid     =null;
			}
		}
		return false;
	}
}

//キーアップ時の処理（nextInput）
var setInputColor = function(evt){
	if($('input.nextinput:focus').length > 0){
		if( $('input.nextinput:focus')[0].value != ''){
			$('input.nextinput:focus').css('background-color',document.getElementById("sbgcolor").value);
		}else{
			$('input.nextinput:focus').css('background-color','');
		}
	}
}

//キーアップ時の処理（キーダウンでは対応不可）
var escCancel = function(evt){
	//入力チェック
	if(!inpCheck(evt)){
		return false;
	}

	if (evt.keyCode == 27 && sendFlag == false) { // Esc
		var targElem = evtToElement(evt);
		var outfname = $('#outfname');
		var infname  = $('#infname');
		var pid  = $('#pid');
		try{
		escKeyFlg = true;
		//sendFlagとTimeoutの処理
		preProcess();
		$.ajax({
			type: "POST",
			url: "param.php",
			data:{ value:'`esc' ,infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
			success: function(msg){
				screenReplace( msg );
				sendFlag = false;
			},
			error: function(jqXHR,textStatus,errorThrown ){
				alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
				console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			}
		});
		}catch(e){
			escKeyFlg = false;
			sendFlag = false;
		}
		return false;
	}
}

var setBlinkToElement = function(changeScreen){
	targetArray = $(changeScreen).find('.blink');
	var ii = 0;
	if(typeof(blinkVisibleFlg) == "undefined" || blinkVisibleFlg == false){
		for(ii=0;ii < targetArray.length;ii++ ){
			targetArray[ii].style.visibility = "visible";
		}
	}else{
		for(ii=0;ii < targetArray.length;ii++ ){
			targetArray[ii].style.visibility = "hidden";
		}
	}
	targetArray = null;
	return changeScreen;
}


//screenのエレメントを順に変更していく
//inputはjQueryオブジェクトを
var replaceScreenElement = function(targScreen,changeScreen){

	var stopFlg = -99;        //
	var remElemArr =new Array();
	//送信済みのinput削除?
	remElemArr = removeSendedInput();
	//プロセス終了時の戻る対応・ステータスバー対応（ジョブの実行状態も込み）

	changeScreen = setBlinkToElement(changeScreen);

//	//画面の要素を順番に比較。違う要素は変更をかける
	//どちらかにインプットが存在しなければ書き換える
	if(targScreen.find('input').length > 0  && changeScreen.find('input').length > 0){
		//最後のインプットを比較。名前が一緒ならとりあえず、OK
		//実行ファイルが変わっても同じ名前でもう一度があったときは書き換え
		(function(){
			var origCount = 0;
			var changCount = 0;
			for(origCount =0;origCount < targScreen[0].children.length ;origCount++ ){
				var curElem = targScreen[0].children[origCount];
				//lineクラスを持っていたら行のテキスト系
				if($(curElem).hasClass('line')){
					//全く同じclassを持っているもので差し替え
					//行のループ
					for(changCount = 0;changCount < changeScreen[0].children.length ;changCount++){
						if(curElem.getAttribute('class') == changeScreen[0].children[changCount].getAttribute('class')){
							//inputが含まれる場合要素をさらに分解して変換
							if($(curElem).find('input').length > 0){
								//inputが入っているところ以外を削除
								//１行中の要素の処理
								for(var i=0;i < curElem.children.length;i++){
									//いったんinputが含まれないところを削除
									if(curElem.children[i].tagName.toLowerCase() != 'input'){
										curElem.removeChild(curElem.children[i]);
										//removeされると配列番号が変わるので
										//ここをjQuery的にremoveしなければ
										i--;
									}else{
										//inputの項目は次の画面含まれていないときだけ削除
										//inputは画面に一つになるようにしなければいけない       みかｎ lastchildを正しい位置に置けばいい？
										var nextViewExistFlg = 0;
										for(var j=0;j < changeScreen[0].children[changCount].children.length;j++){
											var curCoulumn = curElem.children[i].className.match(/\s+f[0-9]+/i);
											var changeCoulumn = changeScreen[0].children[changCount].children[j].className.match(/\s+f[0-9]+/i);

											//中身以外のinputが来た時に飛ばす
											if(curCoulumn == null || changeCoulumn == null){
												break;
											}

											//inputを入れ替えるかどうか
											if((curElem.children[i].name == changeScreen[0].children[changCount].children[j].name )
											&& (curCoulumn[0] == changeCoulumn[0])
											&& remElemArr.indexOf(curElem.children[i].name) == -1){
												//curCoulumn[0] == changeCoulumn[0]で横に同じ名前が続く時の対応
												nextViewExistFlg = 1;

												//エラーで止まっている時はSTOPのInputが来るので
												//一度以上出していたら再び出さない
												if(curElem.children[i].name == "STOP" && BuzzerFlg != 1){
													stopFlg = 1;
												}
											}
										}
										if(nextViewExistFlg == 0)	{
											curElem.removeChild(curElem.children[i]);
											//removeされると配列番号が変わるので
											i--;
										}
									}
								}
								//inputが入っていないところを追加
								for(var j=0;j < changeScreen[0].children[changCount].children.length;j++){
									//inputが含まれないところを追加
									if(changeScreen[0].children[changCount].children[j].tagName.toLowerCase() != 'input'){
										curElem.appendChild(changeScreen[0].children[changCount].children[j]);
										//appendされると配列番号が変わるので
										j--;
									}else{
										//inputの項目は前の画面含まれていないときだけ追加
										var nextViewExistFlg = 0;
										for(var i=0;i < curElem.children.length;i++){
											if(changeScreen[0].children[changCount].children[j].name == curElem.children[i].name){
												nextViewExistFlg = 1;
											}
										}
										if(nextViewExistFlg == 0){
											if(changeScreen[0].children[changCount].children[j].name != "STOP"){
												BuzzerFlg=0;
											}
											curElem.appendChild(changeScreen[0].children[changCount].children[j]);
											//appendされると配列番号が変わるので
											j--;
										}
									}
								}
							}else{
								//inputがないとことは単純に差し替え
								curElem.innerHTML = changeScreen[0].children[changCount].innerHTML;
							}
							//変更候補のelementは削除しておく
							//remove
							changeScreen[0].removeChild(changeScreen[0].children[changCount]);
							break;
						}
					}

				}else{
					//罫線は引き直されていることを考えて一度remove
					targScreen[0].removeChild(curElem);
					//removeされると配列番号が変わるので
					origCount--;
				}
				curElem = null;
			}
			//罫線を引き直し
			var i = 0;
			while(i < changeScreen[0].children.length){
				if(changeScreen[0].children[i].tagName.toLowerCase() == 'div'){
					targScreen[0].appendChild(changeScreen[0].children[i]);
					//appendされると要素が減っていくので
					i--;
				}
				i++;
			}
		})();
	}else{
		targScreen[0].innerHTML = changeScreen[0].innerHTML;
	}

	//画面を作り替えたら画面色再構成
	//buzzuerより前に
	userSetting();

	//初期値を-99としているので
	if(stopFlg > -99 && sendFlag == false){
		(function (){
			buzzerStart("");
			sendFlag = true;
		})();
		//送ったら初期化
		stopFlg=-99;
	}

	//モーダル
	if($('#err-buz').length > 0){
		var soundLength = document.getElementById('err-buz').value;
		buzzerStart(soundLength);
	}
	//音のみ
	if($('#info-buz').length > 0){
		var soundLength = document.getElementById('info-buz').value;
		buzzerStart(soundLength);
	}
}

//要素を入れ替える
function screenReplace(resultTxt){
	var screenObj = $(".screen");
	var changeObj = document.createElement('div');
	changeObj.innerHTML = resultTxt;
//	var changeObj = $(resultTxt);

	changeObj = changeStatus(changeObj);
	if(changeObj == null){
		return 1;
	}
	//取得行のいれかえ
	changeObj = setLineIndex(changeObj);
	replaceScreenElement(screenObj ,$(changeObj));

//	$(changeObj).('*').remove();
	lineTxt   = null;
	screenObj = null;
	changeObj = null;
	//イベントの貼り直し
	reStart();

	return 0;
}

//入力時のエンターをキャンセル
var enterCancel = function (evt){
	switch(evt.Key){
	case 'F10':
	case 'Enter' :
		return false;
		break;
	default :

	}
}

var AnyKeyDownStopBuzzer = function(){
	//sendFlagとTimeoutの処理
	preProcess();
	$.ajax({
		type: "POST",
		url: "param.php",
		data:{ value: '`h' ,infname:$('#infname')[0].value, outfname:$('#outfname')[0].value, pid:$('#pid')[0].value },
			success: function(msg){
			screenReplace( msg );
			sendFlag = false;
			stopFlg = -99;
		},
		error: function(jqXHR,textStatus,errorThrown ){
			alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
		}
	});
	BuzzerFlg = 0
	clearTimeout(BuzzerTimeoutID);
}


var buzzerUntilAnyKeyDown = function(soundLength){
	BuzzerFlg = BuzzerFlg + 1
	if(BuzzerFlg <= soundLength){
	  // base64文字列を貼り付け(音の長さをここでコントロールする必要有り？
	  var base64 = "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"
	  // datauri scheme 形式にして Audio オブジェクトを生成します
	  var sound = new Audio("data:audio/wav;base64," + base64);
	  // 音を鳴らします
	  sound.play();
/*
		var msg = 'エラーが発生しました。\n';
		msg  +=  '管理者に連絡してください。\n';
		msg  +=  soundLength + '';
		alert(msg);
		BuzzerFlg = 1;
*/
	}else{

	}
	if(!escKeyFlg){
		clearTimeout(BuzzerTimeoutID);
		BuzzerTimeoutID=setTimeout('buzzerUntilAnyKeyDown()',50);
	}

}

//beep音の処理
var buzzerStart = function(soundLength){
	var elemTargs = $('.screen input:last');
	//escapeのフラグをりせっとして押されるまで
	escKeyFlg = false;
	addEvent('keydown' ,
			elemTargs[0],
			function evtAnyKeyDown( event ){
				return AnyKeyDownStopBuzzer( event );
			}
	);
	elemTargs = null;
	buzzerUntilAnyKeyDown(soundLength);

}
//TODO:: 入力値をバッファする仕組み
//20161117 add koyama
var InputBuffer = function(event){
	var targElem = evtToElement(event);
//	if(targElem.tagName.toLowerCase().match('intput')){
		// console.log('======'+'not input'+'==='+event.key+'=='+event.type+'==='+ targElem.tagName.toLowerCase() +'===========');
		// console.timeStamp('not input');
		// console.trace();
//	}
}

var inputAddElement = function (){
	var inputElements = $('.screen input');
	for(var i = 0; i < inputElements.length ;i++){
		//次のtab stopへ移動のイベント追加
		if($(inputElements[i]).hasClass('nextinput')){
			addEvent('keydown' ,
					inputElements[i] ,
					function evtNextInput( evt ){
						return nextInput( evt );
					}
			);
			addEvent('keyup' ,
				inputElements[i] ,
				function evtSetInputColor( evt ){
					return setInputColor( evt );
				}
			);
		}
		addEvent('keyup' ,
		inputElements[i] ,
		function evtSetReplaceMode( evt ){
			if(evt.keyCode == 45 && evt.code == "Insert"){
				ReplaceModeFlg = (ReplaceModeFlg + 1) % 2;
			}
			return false;
		}
		);
		addEvent('keypress' ,
				inputElements[i] ,
				function evtEnterCancel( evt ){
					return enterCancel( evt );
				}
		);
	}
	inputElements = null;
	addEvent('keydown' ,
			window.document,
			function evtAnyKeyEvent( evt ){
				return anyKeyEvent( evt );
			}
	)
	addEvent('keyup' ,
			window.document,
			function evtEscCancel( evt ){
				return escCancel( evt );
			}
	)
	//TODO:: 入力値をバッファする仕組みを作る ->出来たらそこから入力を行うように仕様変更
	//20161117 add koyama
	var screenElements = $('.screen');
	addEvent('keypress' ,screenElements[0], InputBuffer )
	screenElements = null;

	//ファンクションキーの機能は使わせないように
	$(window).keydown(function (event){
		//F5 + ctrl
		if((event.key.match(/F[0-9]{1,2}/)) || event.key == 'Alt' ){
			return false;
		}
		return true;
	});
}

//再スタート用
function reStart(){
//	sendFlag = false;
	//イベントの貼り直し
	inputAddElement();
	if(document.activeElement.id != "skSelect"){
		$('input.nextinput:last').focus();
	}

	clearTimeout(TimeoutID);
	TimeoutID = setTimeout('fncChangeScreen()',TimeoutTime);
}

//送信済みのinput削除
//author:koyama
var removeSendedInput = function(){
	var retVal = new Array();
	//入力終了したら、そのエレメントを削除
	if(sendFlag == true){
		var i=0;
		while(0 < $('.screen input').length){
			retVal[i] = $('.screen input:first')[0].name;
			i++;
			//TODO: ここはメモリリーク？
			$('.screen input:first').remove();
		}
	}

	return retVal;
}

//画面に今までに何行とったかを再設定
//画面やresponceに設定がなければ何もせず終了
//author:koyama
//date  :20151005
function setLineIndex(resObj){
	var ii = 0;

	if($(resObj).find('#line_index').length > 0){
		//値は一つ目をwindowの全てのelementに入れる処理にしておく
		$('#line_index')[0].value = $(resObj).find('#line_index')[0].value;
		$(resObj).find('#line_index').remove();
	}
	return resObj;
}

//changeScreenの中からステータスに関連するものを取得して画面に表示
//表示後は不要になるため消去
function changeStatus(changeScreen){
	//changeScreenの中からerror探して表示
	if($(changeScreen).find('.error').length > 0){
		for(var ii=0;ii < $(changeScreen).find('.error').length ;ii++ ){
			//同じ文字列が見つからなかったら
			if(ErrorStrArray.length == 0 || ($(changeScreen).find('.error').length > 0 && $.inArray($(changeScreen).find('.error')[ii].value,ErrorStrArray) == -1)){
				escKeyFlg = false;
				//ステータスバー対応
				var status1 = $("#status1")[0];
				status1.innerHTML = "<span>" + $(changeScreen).find('.error')[0].value + "</span>";
				ErrorStrArray.push($(changeScreen).find('.error')[ii].value);
				status1 = null;
			}
		}
		//あとに影響をおよぼすので削除しておく
		if($(changeScreen).find('#status1Get')[0]){
//			var elemParent = $(changeScreen).find('#status1Get')[0].parentNode;
			$(changeScreen).find('#status1Get').remove();
		}
	}

	//プロセス終了時の戻る対応
	//parentStatusGetが存在している場合parentStatusに代入して
	//changeScreenの中からparentStatusGetを消す
	if($(changeScreen).find('.parentStatusGet').length > 0){
		$("#parentStatus").val($(changeScreen).find('.parentStatusGet')[0].value);
		$(changeScreen).find('.parentStatusGet').remove();
	}

	//pidが取得出来なくても終了したということなので、戻る
	if($('#pid') == void 0 && historyBackFlg == false){
		// location.href = document.referrer;
		historyBackFlg = true;
		history.back();
		return null;
	}
	//プロセス終了時の戻る対応
	//一旦画面に書き出しそれを処理
	//ただしエラーメッセージがあるときはEscキー押下時のみ戻る
	if($('#parentStatus').length > 0 && $('#parentStatus')[0].value == "end"){
		if(ErrorStrArray.length == 0 && historyBackFlg == false){
			// location.href = document.referrer;
			historyBackFlg = true;
			history.back();
			return null;
		}else{
			if(escKeyFlg == true && historyBackFlg == false){
				escKeyFlg = false;
				// location.href = document.referrer;
				historyBackFlg = true;
				history.back();
				return null;
			}
		}
	}

	return changeScreen;
}

function preProcess(){
	sendFlag = true;
	clearTimeout(TimeoutID);
}

//load時に最初にする内容を
var loadTime = function () {
	inputAddElement();

	TimeoutID = setTimeout('fncChangeScreen()',TimeoutTime);
};

addEvent('load',window,loadTime);
