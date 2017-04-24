//戻るボタンにヒストリーバックのイベントを張り付ける
function clickBack() {
	var targetElements = $('.backButton');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("click", targetElements[i] , back);
	}
}

//戻るボタン押下時のイベント
//フォームを作成してPOST送信する
var back = function () {
	
	//画面の戻り先取得(戻るボタンの２番目のクラス名に格納している為）
	var backButton = document.getElementsByName('backButton');
	bBclassName = this.className.split(" ")
	
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./' + bBclassName[1] + ".php");
	
	//選択テーブル
	var selectedTables = document.getElementsByName('selectedTables[]');
	if(selectedTables.length > 0){
		for(var i=0;i < selectedTables.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedTables[i].name);
			sendInp.setAttribute('value',selectedTables[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//結合条件
	var selectedBonds = document.getElementsByName('selectedBonds[]');
	if(selectedBonds.length > 0){
		for(var i=0;i < selectedBonds.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedBonds[i].name);
			sendInp.setAttribute('value',selectedBonds[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//表示項目
	var selectedItemNames = document.getElementsByName('selectedItemNames[]');
	if(selectedItemNames.length > 0){
		for(var i=0;i < selectedItemNames.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedItemNames[i].name);
			sendInp.setAttribute('value',selectedItemNames[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//抽出条件のリスト
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	if(selectedFilters.length > 0){
		for(var i=0;i < selectedFilters.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedFilters[i].name);
			sendInp.setAttribute('value',selectedFilters[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//並べ替えのリスト
	var selectedSorts = document.getElementsByName('selectedSorts[]');
	if(selectedSorts.length > 0){
		for(var i=0;i < selectedSorts.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedSorts[i].name);
			sendInp.setAttribute('value',selectedSorts[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}




addEvent('load',window,clickBack);