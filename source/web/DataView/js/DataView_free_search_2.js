var newCount = 1;

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
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};

var bListNum = { 
	lListTableName: 1 ,
	lListItemName: 2 ,
	lListJapaneseName: 3 ,
	lListS_point: 4 ,
	lListSize: 5,
	lListType: 6 ,
	listBond: 7 ,
	rListTableName: 8,
	rListItemName: 9,
	rListJapaneseName: 10,
	rListS_point: 11,
	rListSize: 12,
	rListType: 13,
	key: 14,
	id: 15
};

//削除ボタンクリック押下時のイベントを張り付ける
function bondDelButton(){
	var nb = $('#bondDelButton');
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
	if(bclickSelElementsVar == undefined){
		errJsCode = "ERRJ0029";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//削除対象行番号を配列に追加
	//（複数削除を想定のため行番号を配列に入れる仕様したが現在は使用していない）
	var bondList = document.getElementById('bondList');
	for(var i=0;i < bondList.children.length;i++){
		if(bondList.children[i].innerHTML == bclickSelElementsVar.innerHTML){
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
			bondList.removeChild(bondList.children[delList[i]]);
		}
	}
	//未選択状態へ戻す
	bclickSelElementsVar = undefined;
	//行ごとの色変
	(function(){
		var targsList = document.getElementById('bondList');
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


//結合ボタン押下イベントを張り付ける
function bondAddButton(){
	var nb = $('#bondAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//結合ボタン押下イベント
//画面左側選択行の情報を画面右側のグループ内に入れる
function addClick(){

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//検索結果リストから行が選択されていなければreturn
	if(lclickSelElementsVar == undefined || rclickSelElementsVar == undefined ){
		errJsCode = "ERRJ0030";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//左右のテーブル名取得
	var lselectTableName = document.getElementById('lTableName');
	var rselectTableName = document.getElementById('rTableName');
	//追加対象のリストを取得
	var bondList = document.getElementById("bondList");
	
	//同じテーブル同士の結合は不可
	if(lselectTableName.value == rselectTableName.value){
		errJsCode = "ERRJ0031";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//右のテーブルと左のテーブルの組み合わせが既に結合条件の中にあるとエラー
	for(var i=0;i < bondList.children.length;i++){
		if((lselectTableName.value == bondList.children[i].children[bListNum['lListTableName']].innerHTML &&
		    rselectTableName.value == bondList.children[i].children[bListNum['rListTableName']].innerHTML ) ||
		   (lselectTableName.value == bondList.children[i].children[bListNum['rListTableName']].innerHTML && 
		    rselectTableName.value == bondList.children[i].children[bListNum['lListTableName']].innerHTML )){
				errJsCode = "ERRJ0032";
				outputMessage(errJsCode,"errMessage");
				return false;
		}
	}
	
	//これから登録するテーブル１とテーブル２のどちらかが既存に登録している結合条件内のテーブルに存在していること
	//例）A join B , C join D の様にテーブルが連結されていないのでエラーとする
	var tableArray = new Array();
	for(var i=0;i < bondList.children.length;i++){
		tableArray.push(bondList.children[i].children[bListNum['lListTableName']].innerHTML);
		tableArray.push(bondList.children[i].children[bListNum['rListTableName']].innerHTML);
	}
	if( tableArray.length != 0 && tableArray.indexOf(lselectTableName.value) < 0 && tableArray.indexOf(rselectTableName.value) < 0 ){
		errJsCode = "ERRJ0033";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	
	//結合条件のリストにテーブル１情報追加
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	s1.title     = '開始位置 : ' + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + '\nサイズ : ' + lclickSelElementsVar.children[lListNum['listSize']].innerHTML + '\n型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'lListItemName bgChItems';
	s2.title     = '開始位置 : ' + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + '\nサイズ : ' + lclickSelElementsVar.children[lListNum['listSize']].innerHTML + '\n型 : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);
	
	s3 = document.createElement('span');
	s3.className = 'hidden lListJapaneseName bgChItems';
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);
	
	s4 = document.createElement('span');
	s4.className = 'hidden lListS_point bgChItems';
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s4);
	s5 = document.createElement('span');
	s5.className = 'hidden lListSize bgChItems';
	s5.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s5);
	s6 = document.createElement('span');
	s6.className = 'hidden lListType bgChItems';
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s6);

	//結合方法
	var bond = document.getElementById("bond1");
	s8 = document.createElement('span');
	//s8.id = 'bondChild';
	s8.className = 'listBond bgChItems';
	if(bond.checked){
		s8.innerHTML = "左を元に結合";
	}else{
		s8.innerHTML = "互いに一致のみ";
	}
	row1.appendChild(s8);

	//結合条件のリストにテーブル２情報追加
	s10 = document.createElement('span');
	s10.className = 'rListTableName bgChItems';
	s10.title     = '開始位置 : ' + rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + '\nサイズ : ' + rclickSelElementsVar.children[rListNum['listSize']].innerHTML + '\n型 : ' + rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	s10.innerHTML = rselectTableName.value;
	row1.appendChild(s10);
	
	s11 = document.createElement('span');
	s11.className = 'rListItemName bgChItems';
	s11.title     = '開始位置 : ' + rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + '\nサイズ : ' + rclickSelElementsVar.children[rListNum['listSize']].innerHTML + '\n型 : ' + rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	s11.innerHTML = rclickSelElementsVar.children[rListNum['listItemName']].innerHTML;
	row1.appendChild(s11);
	
	s12 = document.createElement('span');
	s12.className = 'hidden rListJapaneseName bgChItems';
	s12.innerHTML = rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s12);
	
	s13 = document.createElement('span');
	s13.className = 'hidden rListS_point bgChItems';
	s13.innerHTML = rclickSelElementsVar.children[rListNum['listS_point']].innerHTML;
	row1.appendChild(s13);
	s14 = document.createElement('span');
	s14.className = 'hidden rListSize bgChItems';
	s14.innerHTML = rclickSelElementsVar.children[rListNum['listSize']].innerHTML;
	row1.appendChild(s14);
	s15 = document.createElement('span');
	s15.className = 'hidden rListType bgChItems';
	s15.innerHTML = rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	row1.appendChild(s15);

	//テーブル１とテーブル２のIDを結合して新たなKEYを割り当てる
	s17 = document.createElement('span');
	s17.className = 'hidden key bgChItems';
	s17.innerHTML = lselectTableName.value + '.' + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML + "~" + 
	                rselectTableName.value + '.' + rclickSelElementsVar.children[rListNum['listItemName']].innerHTML;
	row1.appendChild(s17);
	
	//変換
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		ltmpJpaneseName = "";
	}else{
		ltmpJpaneseName = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	}
	
	//テーブル１とテーブル２のIDを結合して新たなidを割り当てる
	s18 = document.createElement('span');
	s18.className = 'bondChildLast hidden id bgChItems';
	s18.innerHTML = lselectTableName.value + '.' + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML + "~" +
	                                               ltmpJpaneseName + "~" +
	                                               lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + "~" +
	                                               lclickSelElementsVar.children[lListNum['listSize']].innerHTML + "~" +
	                                               lclickSelElementsVar.children[lListNum['listType']].innerHTML ;
	if(bond.checked){
		s18.innerHTML = s18.innerHTML + "~" + "LEFTJOIN" + "~";
	}else{
		s18.innerHTML = s18.innerHTML + "~" + "INNERJOIN" + "~";
	}
	
	//変換
	if(trim(rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		rtmpJpaneseName = "";
	}else{
		rtmpJpaneseName = rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML;
	}
	
	s18.innerHTML = s18.innerHTML + rselectTableName.value + '.' + rclickSelElementsVar.children[rListNum['listItemName']].innerHTML + "~" +
	                                                               rtmpJpaneseName + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listSize']].innerHTML + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listType']].innerHTML ;
	row1.appendChild(s18);

	row1.className += "listElem insert add";

	//末尾に追加するための処理
	var bondChildLast = $(".bondChildLast:last");
	if(bondChildLast.length==0){
		bondList.insertBefore(row1,(bondList.hasChildNodes()) ? bondList.childNodes[0] : null);
	}else{
		var parentDiv = bondChildLast[0].parentNode;
		bondList.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//行ごとの色変え用クラス名を追加
	(function(){
		var targsList = document.getElementById('bondList');
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

addEvent("load",window,bondDelButton);
addEvent("load",window,bondAddButton);


//次へボタン押下時のイベントを張り付ける
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , function(event){ return next(event);});
	}
}


//次へボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されているグループと、それにに追加・削除されたユーザ情報を送る
var next = function (event) {

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	var selectedTables = document.getElementsByName('selectedTables[]');

	//入力チェック（すべてのテーブルを使用しているか）
	var bondList = document.getElementById("bondList");
	var tableArray = new Array();
	for(var i=0;i < bondList.children.length;i++){
		tableArray.push(bondList.children[i].children[bListNum['lListTableName']].innerHTML);
		tableArray.push(bondList.children[i].children[bListNum['rListTableName']].innerHTML);
	}
	for(var i=0;i < selectedTables.length ; i++){
		if(tableArray.indexOf(selectedTables[i].value) < 0){
			errJsCode = "ERRJ0034";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_3.php');
	
	//選択テーブル名
	for(var i=0;i < selectedTables.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedTables[i].name);
		sendInp.setAttribute('value',selectedTables[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//結合条件のリスト
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedBonds[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var bondList = document.getElementById('bondList');
	for(var i=0;i < bondList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//テーブル名追加
		sendText = bondList.children[i].children[bListNum['id']].innerHTML;
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


