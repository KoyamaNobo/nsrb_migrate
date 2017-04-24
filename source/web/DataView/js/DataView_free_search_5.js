var newCount = 1;
var nbsp = String.fromCharCode( 160 );

//リストの何番目のスパンに値が入っているか設定
var lListNum = { 
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName : 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};

//抽出条件追加ボタン押下時のイベントを張り付ける
var filterButton = function () {
	var nb = $('.filterButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , filter);
	}
}
//抽出条件追加ボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されている結合テーブル、結合条件、表示項目、現在追加されている抽出条件を送る
var filter = function () {

	var value = document.getElementById('value');//値取得
	var operator = document.getElementById('operator');//演算子取得

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//項目のリストから行が選択されていなければエラー表示
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0038";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	//演算子が入っていないとき
	if(operator.value == ""){
		errJsCode = "ERRJ0039";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	
	//値が入力されていないとき
	if(trim(value.value) == ""){
		errJsCode = "ERRJ0040";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//値の入力値について
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "9" || lclickSelElementsVar.children[lListNum['listType']].innerHTML == "99"){
		//数値かどうか
		if(!input_check_Numeric(trim(value.value))){
			errJsCode = "ERRJ0041";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//桁数が正しいかどうか
		if(!input_check_length(trim(value.value),0,lclickSelElementsVar.children[lListNum['listSize']].innerHTML)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "C9"){
		//数値かどうか
		if(!input_check_Numeric(trim(value.value))){
			errJsCode = "ERRJ0041";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//桁数算出
		max = parseInt(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
		max = (max * 2) - 1;
		
		//桁数が正しいかどうか
		if(!input_check_length(trim(value.value),0,max)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "S9"){
		//数値かどうか（マイナス許可）
		if(!input_check_Numeric_Minus(trim(value.value))){
			errJsCode = "ERRJ0043";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//マイナスの場合マイナスを除去（桁数計算のため）
		var tmp = '';
		if(trim(value.value).substr(0,1) == "-"){
			tmp = trim(value.value).substr(1);
		}else{
			tmp = trim(value.value)
		}
		
		//桁数が正しいかどうか
		if(!input_check_length(tmp,0,lclickSelElementsVar.children[lListNum['listSize']].innerHTML)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "SC9"){
		//数値かどうか（マイナス許可）
		if(!input_check_Numeric_Minus(trim(value.value))){
			errJsCode = "ERRJ0043";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//マイナスの場合マイナスを除去（桁数計算のため）
		var tmp = '';
		if(trim(value.value).substr(0,1) == "-"){
			tmp = trim(value.value).substr(1);
		}else{
			tmp = trim(value.value)
		}
		
		//桁数算出
		max = parseInt(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
		max = (max * 2) - 1;
		//桁数が正しいかどうか
		if(!input_check_length(tmp,0,max)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "N"){
		//この項目（２バイト文字）に対しては検索は許さない
		errJsCode = "ERRJ0044";
		outputMessage(errJsCode,"errMessage");
		return false;
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "X" ||
	          lclickSelElementsVar.children[lListNum['listType']].innerHTML == ""){
		//半角以外不可
		if(!input_check_Alphanumeric_ex(trim(value.value))){
			errJsCode = "ERRJ0045";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//禁止文字列は不可
		if(!input_check_symbol(trim(value.value))){
			errJsCode = "ERRJ0046";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_4.php');
	
	//選択テーブル名
	var selectedTables = document.getElementsByName('selectedTables[]');
	for(var i=0;i < selectedTables.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedTables[i].name);
		sendInp.setAttribute('value',selectedTables[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//結合条件
	var selectedBonds = document.getElementsByName('selectedBonds[]');
	for(var i=0;i < selectedBonds.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedBonds[i].name);
		sendInp.setAttribute('value',selectedBonds[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//表示項目
	var selectedItemNames = document.getElementsByName('selectedItemNames[]');
	for(var i=0;i < selectedItemNames.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedItemNames[i].name);
		sendInp.setAttribute('value',selectedItemNames[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//抽出条件の既存で設定されているリスト
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	for(var i=0;i < selectedFilters.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedFilters[i].name);
		sendInp.setAttribute('value',selectedFilters[i].value);
		sendForm.appendChild( sendInp );
	}
	
	
	
	//抽出条件の下のリスト
	var sendInp = document.createElement('input');
	var sendText = "";
	var value = document.getElementById('value');
	var operator = document.getElementById('operator');
	var andOr1 = document.getElementById('andOr1');
	var andOr2 = document.getElementById('andOr2');
	
	sendInp.setAttribute('name','selectedFilter[]');
	if(andOr1.checked){
		sendText += andOr1.value;
	}else{
		sendText += andOr2.value;
	}
	
	
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listTableName']].innerHTML);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listItemName']].innerHTML);
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		sendText += "~";
	}else{
		sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML);
	}
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listS_point']].innerHTML);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
	if(trim(lclickSelElementsVar.children[lListNum['listType']].innerHTML) == "&nbsp;"){
		sendText += "~";
	}else{
		sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listType']].innerHTML);
	}
	sendText += "~" + operator.value;
	sendText += "~" + trim(value.value);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['id']].innerHTML);
	sendInp.setAttribute('value',sendText);
	sendForm.appendChild( sendInp );
	
	
	
	
	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,filterButton);


//addEvent("load",window, function(){
//							return setHigerHeight('leftID', 'rightSelectID');
//						});

//addEvent("load",window, function(){
//							return setHeight('lcontents','contents' ,'ccontents');
//						});

