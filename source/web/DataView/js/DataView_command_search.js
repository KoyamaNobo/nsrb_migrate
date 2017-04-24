//削除ボタンクリック押下時のイベントを張り付ける
function delButton(){
	var nb = $('#commandDeleteButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}
//削除クリック時のイベント
function delClick(){

	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//検索結果リストが選択されていなければエラー表示
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0024";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//ログインユーザ以外が作成した条件タグは削除させない
	LoginId = document.getElementById("LoginId").value;
	if(lclickSelElementsVar.children[2].innerHTML != LoginId){
		errJsCode = "ERRJ0019";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//確認画面出力
	if(!window.confirm('条件タグ名:['+ lclickSelElementsVar.children[1].innerHTML +'] をDBより削除します。よろしいですか。')){
		return ;
	}
	
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_command_search.php');
	
	//条件タグID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Command_Id');
	sendInp.setAttribute('value',lclickSelElementsVar.children[3].innerHTML);
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
addEvent('load',window,delButton);


//次へボタン押下時のイベントを張り付ける
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}
//次へボタン押下時のイベント
//フォームを作成してPOST送信する
var next = function () {
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//何も選択されていなければメッセージを表示してreturn
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0024";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_disp.php');
	
	//条件タグID
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','selectedCommandId');
	sendInp.setAttribute('value',lclickSelElementsVar.children[3].innerHTML);
	sendForm.appendChild( sendInp );
	
	//プログラム名リスト（今の画面名）
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

