var newCount = 1;
var sendFlg = false;
var errJsCode = '';

//更新ボタンを押したときの処理
function itemUpdate() {
	if(!sendFlg){

		//メッセージ初期化
		errJsCode = "";
		outputMessage("","errMessageModal");
		outputMessage("","successMessage");
	
		//入力チェック
		if(!input_check_item_conf()){
			//errJsCodeはinput_check_item_confにて設定されている
			outputMessage(errJsCode,"errMessageModal");
			return false;
		}
		
		//何も選択されていなければメッセージを表示
		if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0022";
				outputMessage(errJsCode,"errMessageModal");
				return false;
		}

		//フォーム作成
		var sendForm = document.createElement('form');
		sendForm.className = "hidden";
		sendForm.setAttribute('method','post');
		sendForm.setAttribute('action','./DataView_item_conf.php');
		
		//テーブル名取得
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','TableName');
		sendInp.setAttribute('value',lclickSelElementsVar.children[1].innerHTML);
		sendForm.appendChild( sendInp );
		
		//項目名取得
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','ItemName');
		sendInp.setAttribute('value',lclickSelElementsVar.children[2].innerHTML);
		sendForm.appendChild( sendInp );
/*
		//和名取得
		var Japanese_Name = $('#Japanese_Name');
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','JapaneseName');
		sendInp.setAttribute('value',Japanese_Name[0].value.replace(/\s+/g, "")); //半角スペースを除去しておく
		sendForm.appendChild( sendInp );

		//表示・非表示取得
		var tmp;
		var NonDisp_Flg = $('#NonDisp_Flg');
		if($("#NonDisp_Flg:checked").val()) {
			tmp = "1";
		}else{
			tmp = "0";
		}*/
		
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','JapaneseName');
		sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Japanese_Name.value.replace(/\s+/g, "")); //半角スペースを除去しておく
		sendForm.appendChild( sendInp );
		
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','NonDisp_Flg');
		sendInp.setAttribute('value',document.forms[(document.forms.length)-1].NonDisp_Flg.value);
		sendForm.appendChild( sendInp );
		
		//開始位置取得
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','S_point');
		sendInp.setAttribute('value',lclickSelElementsVar.children[5].innerHTML);
		sendForm.appendChild( sendInp );
		
		//サイズ取得
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','Size');
		sendInp.setAttribute('value',lclickSelElementsVar.children[6].innerHTML);
		sendForm.appendChild( sendInp );
		
		//型取得
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','Type');
		if(lclickSelElementsVar.children[7].innerHTML == '&nbsp;'){
			sendInp.setAttribute('value','');
		}else{
			sendInp.setAttribute('value',lclickSelElementsVar.children[7].innerHTML);
		}
		sendForm.appendChild( sendInp );
		
		//フォーム実行
		document.body.appendChild( sendForm );
		sendFlg = true;
		sendForm.submit();
	}
	return false;

}

//更新ボタン押下時のイベントを張り付ける
var itemUpdateButton = function () {
	var nb = $('.itemUpdateButton');
	var ii = 0;
	for(ii = 0 ; ii < nb.length;ii++){
		addEvent("click", nb[ii] , itemUpdate);
	}
}
addEvent('load',window,itemUpdateButton);




//和名登録する際の入力チェック
function input_check_item_conf() {

	//和名取得
	Japanese_Name = document.getElementById("Japanese_Name").value;
	Japanese_Name = Japanese_Name.replace(/\s+/g, ""); //半角スペースを除去する

	//文字数チェック
	if(!input_check_length(Japanese_Name,0,20)){ //20文字までとする（ただし表示上８文字以上になると乱れる・・・）
		errJsCode = "ERRJ0020";
		return false;
	}
	//禁止文字チェック
	if(!input_check_symbol(Japanese_Name)){ 
		errJsCode = "ERRJ0021";
		return false;
	}
	return true;
}

