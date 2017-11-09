var sendTime = 0;

//特殊キーのイベント貼り付け
var specialKeyLoad = function () {
	var skButtonElem = $('#skButton')[0];
	if(typeof skButtonElem != "undefined"){
		addEvent('click' , skButtonElem ,funcSkButton );
	}
};

//入力キー（特殊）によってskExecファンクションを実行
//  戻り値:特殊キーの場合true
//        :特殊キーではない場合false
function funcSpecialKey(evt){
	var targElem = evtToElement(evt);
	if(sendFlag == false){
		if(evt.keyCode == 115 && evt.ctrlKey == true && evt.shiftKey == true){
			//停止(CTRL + SHIFT + F4)
			funcSkExec("CS4",evt);
			return true;
		}else if(evt.keyCode == 116 && evt.ctrlKey == true && evt.shiftKey == true){
			//再開(CTRL + SHIFT + F5)
			funcSkExec("CS5",evt);
			return true;
		}else if(evt.keyCode == 119 && evt.ctrlKey == true){
			//プロ放棄(CTRL + F8)
			funcSkExec("C8",evt);
			return true;
		}else if(evt.keyCode == 120 && evt.ctrlKey == true){
			//業務放棄(CTRL + F9)
			funcSkExec("C9",evt);
			return true;
		}else{
			return false;
		}
	}else{
		return false;
	}
};

//画面切り離し系
//switchするときはtrue,しない時はfalse
function screenSwitch(evt){
	//画面切り離し系の処理はその他と切り分け
	if(evt.keyCode == 112 && evt.ctrlKey == true){
		funcF1Button();
	}else if(evt.keyCode == 113 && evt.ctrlKey == true){
		//F2+Ctrl(画面切離)
		funcF2Button();
	}
	return false;
}

//Ctrl+F1
function funcF1Button(){
	//F1+Ctrl(画面再接続)
	if($.find('.modal-content').length >= 1){
		screensSelect();
	}else{
		//modalが出ているときはoverlayが存在
		if($.find('.modal-overlay').length > 0){
			$('.modal-overlay').click();
		}
	}
}

//Ctrl+F2
function funcF2Button(){
	//?が無いのはありえないはず
	var now = new Date();
	if(now.getTime() > sendTime){
		sendTime = now.getTime() + 300;
		window.open(window.location.href.slice(0,window.location.href.indexOf('?')));
		return true;
	}
}

//特殊キー用のボタンを押したときの処理
function funcSkButton(evt){
	var skSelectElem = $('#skSelect')[0];
	if(skSelectElem.value != "" ){
		funcSkExec(skSelectElem.value,evt);
	}
}

//特殊キー実行（keyのパラメータにより処理分岐）
//  key：C9のとき業務放棄
//       C8のときプロ放棄
//       CS5のとき再開
//       CS4のとき停止
function funcSkExec(key,evt){
	var pid  = $('#pid');
	var parentStatus  = $('#parentStatus');
	var targElem = evtToElement(evt);
	var skButtonElem = $('#skButton')[0];

	//実行ジョブが終了していたら特殊キー実行しない
	if(parentStatus[0].value == "end" ){
		return true;
	}

	//Ctrl+F1はAjax軽処理ではないのでここで抜ける
	if(key == "C1"){
		funcF1Button();
		return true;
	}
	//Ctrl+F2はAjax軽処理ではないのでここで抜ける
	if(key == "C2"){
		funcF2Button();
		return true;
	}

	//業務放棄(CTRL + F9)
	//プロ放棄(CTRL + F8)
	//再開(CTRL + SHIFT + F5)
	//停止(CTRL + SHIFT + F4)
	sendFlag = true;
	$.ajax({
		type: "POST",
		url: "sk.php",
		data:{ value:key , pid:pid[0].value },
		success: function(msg){
			//$("#status1").html("");
			sendFlag = false;
		},
		error: function(){
			sendFlag = false;
			alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
		}
	});

	return true;
}

addEvent('load',window,specialKeyLoad);
