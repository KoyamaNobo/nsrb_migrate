var newCount = 1;

var rclickSelElementsVar;

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

//左のリストの何番目のスパン値が入っているか設定
var rListNum = { 
	lListTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listSort: 5 ,
	listS_point: 6 ,
	listSize: 7 ,
	listType: 8 ,
	listNext: 9 ,
	id: 10 
};

//削除ボタンクリック押下時のイベントを張り付ける
function sortDelButton(){
	var nb = $('#sortDelButton');
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
		errJsCode = "ERRJ0048";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//削除対象行番号を配列に追加
	//（複数削除を想定のため行番号を配列に入れる仕様したが現在は使用していない）
	var sortList = document.getElementById('sortList');
	for(var i=0;i < sortList.children.length;i++){
		if(sortList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
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
			sortList.removeChild(sortList.children[delList[i]]);
		}
	}
	//未選択状態へ戻す
	rclickSelElementsVar = undefined;
	//偶数列と奇数列で色を変える
	(function(){
		var targsList = document.getElementById('sortList');
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
function sortAddButton(){
	var nb = $('#sortAddButton');
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
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0049";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	var disp = document.getElementById("sortList");    //追加対象のテーブルを取得
	
	//登録したいソート順と既に登録されているユーザを比較
	//既に登録されているユーザであればメッセージを表示
	for(var i=0;i < disp.children.length;i++){
		var targetIdhold = lclickSelElementsVar.children[rListNum['listId']].innerHTML;
		var targetIdTemp = disp.children[i].children[0].innerHTML ;
		var counter = 0;
		for(var j=0;j < disp.children[i].children.length;j++){
			if(disp.children[i].children[j].nodeType == 1){
				if(counter == rListNum['listId']){
					targetIdTemp = disp.children[i].children[j].innerHTML;
				}
				counter++;
			}
		}
		if(!disp.children[i].getAttribute('class').match('hidden')){
			if(targetIdhold == targetIdTemp){
				errJsCode = "ERRJ0050";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
	}
	
	//右のリストに情報追加
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s1.title     = '型 :  ';
	}else{
		s1.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s1.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s1.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s1.innerHTML = lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	//s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'listItemName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s2.title     = '型 :  ';
	}else{
		s2.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s2.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s2.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);
	
	s3 = document.createElement('span');
	s3.className = 'hidden listJapaneseName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s3.title     = '型 :  ';
	}else{
		s3.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s3.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s3.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);
	
	s4 = document.createElement('span');
	s4.className = 'hidden listId bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s4.title     = '型 :  ';
	}else{
		s4.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s4.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s4.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s4);
	
	s5 = document.createElement('span');
	s5.className = 'listSort bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s5.title     = '型 :  ';
	}else{
		s5.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s5.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s5.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	var sort = document.getElementById("sort");
	if(sort.value=="asc"){
		s5.innerHTML = "昇順";
	}else{
		s5.innerHTML = "降順";
	}
	row1.appendChild(s5);
	
	s6 = document.createElement('span');
	s6.className = 'hidden listS_point right bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s6.title     = '型 :  ';
	}else{
		s6.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s6.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s6.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s6);
	
	s7 = document.createElement('span');
	s7.className = 'hidden listSize right bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s7.title     = '型 :  ';
	}else{
		s7.title     = '型 : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s7.title     += '\n開始位置 ：'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s7.title     += '\nサイズ ：'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s7.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s7);
	
	s8 = document.createElement('span');
	s8.className = 'hidden listType bgChItems';
	s8.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s8);
	
	//次の画面へ引き継ぐ用
	s9 = document.createElement('span');
	s9.className = 'hidden listNext bgChItems';
	//s9.innerHTML = lselectTableName.value;
	s9.innerHTML = lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		s9.innerHTML = s9.innerHTML + "~";
	}else{
		s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	}
	s9.innerHTML = s9.innerHTML + "~" + sort.value;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s9.innerHTML = s9.innerHTML + "~";
	}else{
		s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	row1.appendChild(s9);
	
	s10 = document.createElement('span');
	s10.className = 'hidden sortChildLast id bgChItems';
	s10.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s10);
	
	row1.className += "listElem insert add";
	
	//末尾に追加するための処理
	var sortChildLast = $(".sortChildLast:last");
	if(sortChildLast.length==0){
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		var parentDiv = sortChildLast[0].parentNode;
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//行ごとの色変え用クラス名を追加
	(function(){
		var targsList = document.getElementById('sortList');
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
addEvent("load",window,sortDelButton);
addEvent("load",window,sortAddButton);

//検索開始ボタン押下時のイベントを張り付ける
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}
//検索開始ボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されているグループと、それにに追加・削除されたユーザ情報を送る
var next = function () {
	
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_disp.php');
	
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
	
	//抽出条件
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	for(var i=0;i < selectedFilters.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedFilters[i].name);
		sendInp.setAttribute('value',selectedFilters[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//並べ替え設定のリスト
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedSorts[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var sortList = document.getElementById('sortList');
	for(var i=0;i < sortList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//テーブル名追加
		sendText += trim(sortList.children[i].children[rListNum['listNext']].innerHTML);
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}
	
	//プログラム名取得（現在の画面名）
	var pgName = document.getElementById('pgName');
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',pgName.name);
	sendInp.setAttribute('value',pgName.value);
	sendForm.appendChild( sendInp );
	
	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);
