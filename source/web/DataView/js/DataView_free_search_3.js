var newCount = 1;
var rclickSelElementsVar;
var lclickSelElementsVar = new Array();
//左のリストの何番目のスパン値が入っているか設定
var lListNum = { 
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};
//右のリストの何番目のスパン値が入っているか設定
var rListNum = { 
	lListTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId : 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};
//削除ボタンクリック押下時のイベントを張り付ける
function dispDelButton(){
	var nb = $('#dispDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}
//削除ボタンのイベント
function delClick(){
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	var delList = new Array();
	//何も選択されていなければメッセージを表示
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0035";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//削除対象行番号を配列に追加
	//（複数削除を想定のため行番号を配列に入れる仕様したが現在は使用していない）
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length;i++){
		if(rightList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
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
			rightList.removeChild(rightList.children[delList[i]]); 
		}
	}
	//未選択状態へ戻す
	rclickSelElementsVar = undefined;
	//偶数列と奇数列で色を変える
	(function(){
		var targsList = document.getElementById('rightList');
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

//追加ボタン押下イベントを張り付ける
function dispAddButton(){
	var nb = $('#dispAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//追加ボタン押下イベント
//画面左側選択行の情報を画面右側のグループ内に入れる
function addClick(){
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//左のリストが何も選択されていなければメッセージを表示
	if(typeof lclickSelElementsVar == 'undefined' || lclickSelElementsVar.length == 0){
		errJsCode = "ERRJ0036";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//追加対象のリストを取得
	var disp = document.getElementById("rightList");

	//既に登録されている項目であればメッセージを表示
	for(var i=0;i < disp.children.length;i++){
		//ここはクリック
		var targetIdhold = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
		var targetIdTemp = disp.children[i].children[0].innerHTML ;
		var counter = 0;
		for(var j=0;j < disp.children[i].children.length;j++){
			if(disp.children[i].children[j].nodeType == 1){
				if(counter == lListNum['listId']){
					targetIdTemp = disp.children[i].children[j].innerHTML;
				}
				counter++;
			}
		}
		
		if(!disp.children[i].getAttribute('class').match('hidden')){
			if(targetIdhold == targetIdTemp){
				errJsCode = "ERRJ0051";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
	}
	
	//右のリストに情報を追加
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	lselectTableName = document.getElementById('lTableName');
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	s1.title     = '型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'listItemName bgChItems';
	s2.title     = '型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);

	s3 = document.createElement('span');
	s3.className = 'listJapaneseName bgChItems';
	s3.title     = '型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);

	s4 = document.createElement('span');
	s4.className = 'hidden listId bgChItems';
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s4);
	
	s5 = document.createElement('span');
	s5.className = 'listS_point right bgChItems';
	s5.title     = '型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s5.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s5);
	
	s6 = document.createElement('span');
	s6.className = 'listSize right bgChItems';
	s6.title     = '型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s6);
	
	s7 = document.createElement('span');
	s7.className = 'hidden listType bgChItems';
	s7.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s7);
	
	s8 = document.createElement('span');
	s8.className = 'hidden rightChildLast id bgChItems';
	s8.innerHTML = lclickSelElementsVar.children[lListNum['id']].innerHTML;
	row1.appendChild(s8);
	
	row1.className += "listElem insert add";

	//末尾に追加するための処理
	var rightChildLast = $(".rightChildLast:last");
	if(rightChildLast.length==0){
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		var parentDiv = rightChildLast[0].parentNode;
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//行ごとの色変え用クラス名を追加
	(function(){
		var targsList = document.getElementById('rightList');
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
	//追加した行にクリックイベント貼り付け
	addEvent("click",row1,setClickElems);
}
addEvent("load",window,dispDelButton);
addEvent("load",window,dispAddButton);


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

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

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
	
	//表示項目のリスト
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedItemNames[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	dispCount = 0;
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		sendText += trim(rightList.children[i].children[rListNum['lListTableName']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listItemName']].innerHTML);
		if(trim(rightList.children[i].children[rListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
			sendText += "~";
		}else{
			sendText += "~" + trim(rightList.children[i].children[rListNum['listJapaneseName']].innerHTML);
		}
		sendText += "~" + trim(rightList.children[i].children[rListNum['listId']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listS_point']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listSize']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listType']].innerHTML);
		//テーブル名追加
		
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
		
		dispCount++;
	}

	//表示項目が１つ以上ないとエラー
	if(dispCount < 1){
		errJsCode = "ERRJ0037";
		outputMessage(errJsCode,"errMessage");
		$(sendForm).remove();//フォームが残ってしまうため削除
		return false;
	}

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);
