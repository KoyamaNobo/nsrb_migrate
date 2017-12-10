//インプットボックスのクラスによって入力チェックを分ける
//  「SNUMERC」の場合：sNumericCheckを呼び出す
//  「NUMERC」の場合：numericCheckを呼び出す
//   戻り値 true :入力チェック成功時 または 入力チェックを行わないとき
//          false:入力チェック失敗時
function inpCheck(evt){
	return elementInpCheck(evtToElement(evt));
}

//インプットボックスのクラスによって入力チェックを分ける
//「SNUMERC」の場合：sNumericCheckを呼び出す
//「NUMERC」の場合：numericCheckを呼び出す
//戻り値 true :入力チェック成功時 または 入力チェックを行わないとき
//      false:入力チェック失敗時
function elementInpCheck(targElem) {
	tmp = $(targElem)[0].className;
	if (tmp) {
		if (tmp.match(/SNUMERC/)) {
			if (!sNumericCheck(targElem.value)) {
				// 入力チェックにエラーがあるとき
				return false;
			}
		} else if (tmp.match(/NUMERC/)) {
			if (!numericCheck(targElem.value)) {
				// 入力チェックにエラーがあるとき
				return false;
			}
		}
	}
	return true;
}

//数値かチェックする（正のみ）
//  戻り値 true :入力チェック成功時
//         false:入力チェック失敗時
function numericCheck(val){
	if (val.match(/^( )*-?[0-9]*.?[0-9]*( )*$/)) {
		$("#status6").html( "<span></span>");
		return true;
	} else {
		$("#status6").html( "<span>NUMERC</span>");
		userSetting();
		return false;
	}
}

//数値かチェックする（正・負ともに共用）
//  戻り値 true :入力チェック成功時
//         false:入力チェック失敗時
function sNumericCheck(val){
	if (val.match(/^( )*-?[0-9]*.?[0-9]*( )*$/)) {
		$("#status6").html( "<span></span>");
		return true;
	} else {
		$("#status6").html( "<span>NUMERC</span>");
		userSetting();
		return false;
	}
}

//インプットボックスのクラスがSNUMERCかNUMERCのとき
//入力値のフォーマット変換をする（小数点を付加する）
//  例）「SNUMERC5V2」-1 → -0.01
//  例）「NUMERC5V2」 10 →  0.1
function inpFormat(evt){
	return elementInpFormat(evtToElement(evt));
}


//インプットボックスのクラスがSNUMERCかNUMERCのとき
//入力値のフォーマット変換をする（小数点を付加する）
//例）「SNUMERC5V2」-1 → -0.01
//例）「NUMERC5V2」 10 →  0.1
function elementInpFormat(targElem){
	var res = targElem.value;
	var minusFlg = false;

	formatFlg = false;

	//入力がない場合は何もしない
	if(res.length == 0){
		return res;
	}

	tmp = $(targElem)[0].className;

	var found1 = tmp.match(/SNUMERC[\d]+V[\d]+/);
	if (found1){
		className = found1[0].substr(7);
		formatFlg = true;
	}else{
		var found2 = tmp.match(/NUMERC[\d]+V[\d]+/);
		if (found2) {
			className = found2[0].substr(6);
			formatFlg = true;
		}
	}

	if(formatFlg){
		return res;
	}
	return res;
}
