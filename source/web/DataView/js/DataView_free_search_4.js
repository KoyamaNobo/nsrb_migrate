var newCount = 1;
var nbsp = String.fromCharCode( 160 );

var rclickSelElementsVar;

//削除ボタンクリック押下時のイベントを張り付ける
function filterDelButton(){
	var nb = $('#filterDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}

//上選択行削除クリック時のイベントはりつけ
function delClick(){
	var delList = new Array();
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//何も選択されていなければメッセージを表示してreturn
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0047";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//削除対象行番号を配列に追加
	//（複数削除を想定のため行番号を配列に入れる仕様したが現在は使用していない）
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length;i++){
		if(filterList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
			delList.push(i);
		}
	}
	//削除行を後ろから削除していく
	if(delList.length > 0){
		//配列を降順に並べる
		delList.sort(
			function(a,b){
				if( a < b ) return 1;
				if( a > b ) return -1;
				return 0;
			}
		);
		//後ろから削除
		for(var i=0;i < delList.length;i++){
			filterList.removeChild(filterList.children[delList[i]]);
		}
	}
	//未選択状態へ戻す
	rclickSelElementsVar = undefined;
	//偶数列と奇数列で色を変える
	(function(){
		var targsList = document.getElementById('filterList');
		var elems = getElementsByClassName( targsList, 'listElem');
		var oddFlg = -1;
		for( var k=0;k < elems.length;k++ ){
			if(!elems[k].getAttribute('class').match('hidden')){
				removeClass(elems[k] , 'odd');
				oddFlg *= -1;
				if(oddFlg == -1){
					elems[k].className += ' odd';
				}
			}
		}
	})();
}
addEvent("load",window,filterDelButton);

//次へボタン押下時のイベントを張り付ける
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}
//次へボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されているグループと、それにに追加・削除されたユーザ情報を送る
var next = function () {
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_6.php');
	
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
	
	//抽出条件のリスト
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedFilters[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		//テーブル名追加
		sendText = trim(filterList.children[i].children[10].innerHTML);
		sendText = sendText.replace( nbsp, " " );
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);


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
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_5.php');
	
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
	
	//抽出条件のリスト
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedFilters[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//テーブル名追加
		sendText = trim(filterList.children[i].children[10].innerHTML);
		sendText = sendText.replace( nbsp, " " );
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,filterButton);


