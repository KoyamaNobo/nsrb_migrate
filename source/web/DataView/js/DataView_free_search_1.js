//左のリストの何番目のスパン値が入っているか設定
var lListNum = { 
	listTableName: 1 ,
	listId: 2 ,
	id: 3
};
//右のリストの何番目のスパン値が入っているか設定
var rListNum = { 
	listTableName: 1 ,
	listId: 2 ,
	id: 3
};

//削除ボタンクリック押下時のイベントを張り付ける
function delButton(){
	var nb = $('#tableDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}

//右選択行削除クリック時のイベント
function delClick(){
	var viewElem = new Array();
	var delList = new Array();
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//何も選択されていなければメッセージを表示してreturn
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0027";
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

//右へ追加ボタン押下イベントを張り付ける
function addButton(){
	var nb = $('#tableAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//右へ追加ボタン押下イベント
//画面左側選択行の情報を画面右側のグループ内に入れる
function addClick(){
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//左のリストから行が選択されていなければreturn
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0025";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//右の全リスト取得
	var disp = document.getElementById("rightList");
	
	//登録したいテーブルと既に登録されているテーブルを比較
	for(var i=0;i < disp.children.length;i++){
		var targetIdhold = lclickSelElementsVar.children[lListNum['id']].innerHTML;
		var targetIdTemp = disp.children[i].children[rListNum['id']].innerHTML;
		
		if(targetIdhold == targetIdTemp){
			//既に登録済みの場合メッセージを表示
			errJsCode = "ERRJ0026";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}
	
	//登録する要素を作成
	var row1 = document.createElement("div");                     //追加用のdiv要素作成
	row1.innerHTML = lclickSelElementsVar.innerHTML;
	row1.children[lListNum['id']].className += " rightListLast "; //最終行を判定するためLastとする
	row1.className += "listElem insert add";

	//末尾の行を取得
	var rightListLast = $(".rightListLast:last");
	if(rightListLast.length==0){
		//右のリストに何も登録されていないとき
		//先頭へ追加
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		//右のリストに既に何か登録されている時
		var parentDiv = rightListLast[0].parentNode;
		//末尾へ追加
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
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
	//追加した行へ選択（クリック）したときのイベントを張り付ける
	addEvent("click",row1,setClickElems);
}

addEvent("load",window,delButton);
addEvent("load",window,addButton);



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
	var selectCount = 0;
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_2.php');
	
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedTables[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		//テーブル名追加
		sendText = rightList.children[i].children[lListNum['id']].innerHTML;
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
		selectCount++;
	}
	
	//選択したテーブルが２つ以上ないとエラー
	if(selectCount < 2){
		errJsCode = "ERRJ0028";
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



