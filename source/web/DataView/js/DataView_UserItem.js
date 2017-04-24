var newCount = 1;
var errJsCode = '';

function typeCheck(typeText){
	var retVal=false;
	if(typeText.match('^(X|N|S?C?9+V?9*)$')){
		retVal = true;
	}
	return retVal;
}


//和名登録する際の入力チェック
function input_check_item_conf() {
	//テーブル名
	tableName = document.getElementById("Table_Name").value;
	//文字数チェック //とりあえず10文字
	if(!input_check_length(tableName,1,10)){
		errJsCode = "ERRJ0066";
		return false;
	}
	//文字種チェック
	if(!tableName.match(/^[-a-zA-Z0-9]+$/)){
		errJsCode = "ERRJ0058";
		return false;
	}

	//和名取得
	Japanese_Name = document.getElementById("Item_Japanese_Name").value;
	Japanese_Name = Japanese_Name.replace(/\s+/g, ""); //半角スペースを除去する
	//文字数チェック
	if(!input_check_length(Japanese_Name,1,20)){ //20文字までとする（ただし表示上８文字以上になると乱れる・・・）
		errJsCode = "ERRJ0067";
		return false;
	}
	//禁止文字チェック
	if(!input_check_symbol(Japanese_Name)){
		errJsCode = "ERRJ0068";
		return false;
	}

	//開始位置チェック
	if(!document.forms[(document.forms.length)-1].S_point.value.match('^[0-9]+$')){
		//数字か
		errJsCode = "ERRJ0062";
		return false;
	}
	if(document.forms[(document.forms.length)-1].S_point.value <= 0 || document.forms[(document.forms.length)-1].S_point.value > 4096){
		//有効値範囲内か
		errJsCode = "ERRJ0063";
		return false;
	}

	//サイズチェック
	if(!document.forms[(document.forms.length)-1].Size.value.match('^[0-9]+$')){
		//数字か
		errJsCode = "ERRJ0069";
		return false;
	}
	if(document.forms[(document.forms.length)-1].Size.value <= 0 || document.forms[(document.forms.length)-1].S_point.value > 4096){
		//有効値範囲内か
		errJsCode = "ERRJ0070";
		return false;
	}

	//型チェック
	if(!typeCheck(document.forms[(document.forms.length)-1].Data_Type.value)){
		errJsCode = "ERRJ0061";
		return false;
	}

	return true;
}

function update(){
	regist('update');
}

function add(){
	regist('add');
}

//更新ボタンを押したときの処理
function regist(modeStr) {
	//メッセージ初期化
	errJsCode = "";
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");
	////////////////////////////////////////////////////////////////////////////入力チェック
	if(!input_check_item_conf()){
		//errJsCodeはinput_check_item_confにて設定されている
//		//キーボード操作などにより、オーバーレイが多重起動するのを防止する
//		$(this).blur() ;	//ボタンからフォーカスを外す
//		//[$modal]をフェードアウトさせる
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
	////////////////////////////////////////////////////////////////////////////フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./users_item.php');
	////////////////////////////////////////////////////////////////////////////テーブル名取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','TableName');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Table_Name.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////拡張項目ID
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Extension_Id');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Extension_Id.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////項目論理名
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','JapaneseName');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Item_Japanese_Name.value.replace(/\s+/g, "")); //半角スペースを除去しておく
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////項目論理名
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].User_Id.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////優先表示フラグ
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','NonDisp_Flg');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].NonDisp_Flg.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////開始位置取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','S_point');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].S_point.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////サイズ取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Size');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Size.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////型取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Data_Type');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Data_Type.value);

	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//削除クリック時のイベント
function del(){

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//何も選択されていなければメッセージを表示
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0060";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//権限がなければ削除させない
	//hiddenのinputから取得
	Authority_flg = document.getElementById("Authority_flg").value;
	if(Authority_flg != "1" && (lclickSelElementsVar.children[8].innerHTML != $('#LoginId')[0].value)){
		errJsCode = "ERRJ0015";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//削除されている時はメッセージを表示
	if(lclickSelElementsVar.children[5].innerHTML == '削除'){
		errJsCode = "ERRJ0018";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	var strMsg = '';
	strMsg += 'テーブル名:['+ lclickSelElementsVar.children[2].innerHTML + '], ';
	strMsg += '項目名:['+ lclickSelElementsVar.children[3].innerHTML + '] を' + "\n" ;
	strMsg += 'データベースより削除します。よろしいですか。';
	//確認画面出力
	if(!window.confirm(strMsg)){
		return ;
	}

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./users_item.php');

	//ユーザID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Extension_Id');
	sendInp.setAttribute('value',lclickSelElementsVar.children[1].innerHTML);
	sendForm.appendChild( sendInp );

	//処理フラグ
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','del');
	sendForm.appendChild( sendInp );

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//型説明
function setTypeExplain(event){
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	var text = $(srcElem).attr('data-text');
	$(srcElem.parentNode).append('<div class="explain-tooltips">'+text+'</div>');
}
function delTypeExplain(event){
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	$(srcElem.parentNode).find(".explain-tooltips").remove();
}


//削除ボタンクリック押下時のイベントを張り付ける
function thisDocument(){
	var nb = $('.delButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , del);
	}

	addEvent("focus", document.forms[(document.forms.length)-1].Data_Type , function(event){ return setTypeExplain(event);});
	addEvent("blur", document.forms[(document.forms.length)-1].Data_Type , function(event){ return delTypeExplain(event);});
}

addEvent("load",window,thisDocument);
