var lclickSelElementsVar;
var errJsCode;

//削除ボタンクリック押下時のイベントを張り付ける
function delButton(){
	var nb = $('.delButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , del);
	}
}

//削除クリック時のイベント
function user_del(){

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//ユーザID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
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

//削除クリック時のイベント
function del(){
/*
	var strMsg = '';
	strMsg += 'ID:['+ lclickSelElementsVar.children[1].innerHTML + ']';
	strMsg += 'をデータベースより削除します。よろしいですか。';
	//確認画面出力
	if(!window.confirm(strMsg)){
		return ;
	}
*/
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//ユーザID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
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

//追加ボタン押下イベント
//画面左側選択行の情報を画面右側のグループ内に入れる
function add() {

	//メッセージ初期化
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");

	//権限がなければ追加させない
	//hiddenのinputから取得
	Authority_flg = document.getElementById("Authority_flg").value;
	if(Authority_flg != "1"){
		errJsCode = "ERRJ0017";
		//キーボード操作などにより、オーバーレイが多重起動するのを防止する
		$(this).blur() ;	//ボタンからフォーカスを外す
		//[$modal]をフェードアウトさせる
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}

	//入力チェック
	if(!input_check_insert('add')){
		//キーボード操作などにより、オーバーレイが多重起動するのを防止する
		$(this).blur() ;	//ボタンからフォーカスを外す
		//[$modal]をフェードアウトさせる
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}

	//確認画面出力
	var User_Id = document.forms[0].User_Id;
	var strMsg = '';
	strMsg += 'ユーザID:'+ User_Id.value +'をデータベースへ登録します。よろしいですか。';
	if(!window.confirm(strMsg)){
		return ;
	}

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//ユーザID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Id.name);
	sendInp.setAttribute('value',User_Id.value);
	sendForm.appendChild( sendInp );

	//パスワード取得
	var User_Password = document.forms[0].User_Password;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Password.name);
	sendInp.setAttribute('value',User_Password.value);
	sendForm.appendChild( sendInp );

	//確認用パスワード取得
	var User_Conf = document.forms[0].User_Conf;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Conf.name);
	sendInp.setAttribute('value',User_Conf.value);
	sendForm.appendChild( sendInp );

	//権限取得
	var Authority_Flg = document.forms[0].userAuthority;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','userAuthority');
	sendInp.setAttribute('value',Authority_Flg.value);
	sendForm.appendChild( sendInp );


	//表示件数取得
	var Disp_Num = document.forms[0].Disp_Num;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Disp_Num.name);
	sendInp.setAttribute('value',Disp_Num.value);
	sendForm.appendChild( sendInp );

	//処理フラグ
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','add');
	sendForm.appendChild( sendInp );

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//更新ボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されているグループと、それにに追加・削除されたユーザ情報を送る
function update() {

	//メッセージ初期化
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");

	//入力チェック
	if(!input_check_insert('upd')){
		outputMessage(errJsCode,"errMessageModal");
		//キーボード操作などにより、オーバーレイが多重起動するのを防止する
		$(this).blur() ;	//ボタンからフォーカスを外す
		//[$modal]をフェードアウトさせる
//		$("#modal").fadeOut("slow");
		return false;
	}

	//確認画面出力
	var User_Id = document.forms[0].User_Id;
	var strMsg = '';
	strMsg += 'ユーザID:'+ User_Id.value +'を更新します。よろしいですか。';
	if(!window.confirm(strMsg)){
		//キーボード操作などにより、オーバーレイが多重起動するのを防止する
		$(this).blur() ;	//ボタンからフォーカスを外す
		//[$modal]をフェードアウトさせる
//		$("#modal").fadeOut("slow");
		return ;
	}

	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.reset();
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	//フォームのリンク先設定
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//ユーザID取得
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Id.name);
	sendInp.setAttribute('value',User_Id.value);
	sendForm.appendChild( sendInp );

	//パスワード取得
	var User_Password = document.forms[0].User_Password;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Password.name);
	sendInp.setAttribute('value',User_Password.value);
	sendForm.appendChild( sendInp );

	//パスワード確認用取得
	var User_Conf = document.forms[0].User_Conf;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Conf.name);
	sendInp.setAttribute('value',User_Conf.value);
	sendForm.appendChild( sendInp );

	//権限取得
	var Authority_Flg = document.forms[0].userAuthority;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Authority_Flg');
	sendInp.setAttribute('value',Authority_Flg.value);

	sendForm.appendChild( sendInp );


	//表示件数取得
	var Disp_Num = document.forms[0].Disp_Num;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Disp_Num.name);
	sendInp.setAttribute('value',Disp_Num.value);
	sendForm.appendChild( sendInp );

	//処理フラグ
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','mod');
	sendForm.appendChild( sendInp );

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;

}

//戻るボタンクリック押下時のイベントを張り付ける
function backButton(){
	var nb = $('.backButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , back);
	}
}

//戻るボタン押下時のイベント
//フォームを作成してPOST送信する
//選択されているグループと、それにに追加・削除されたユーザ情報を送る
function back() {
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','get');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

addEvent("load",window,delButton);
addEvent("load",window,backButton);



//入力チェック
function input_check_insert(modeStr) {

	//エラーメッセージ初期化
	errJsCode = "";
	outputMessage(errJsCode,"errMessageModal");
	outputMessage("","successMessage");

	//値取得
	User_Id = document.forms[0].User_Id.value;
	User_Password = document.forms[0].User_Password.value;
	User_Conf = document.forms[0].User_Conf.value;
	Disp_Num = document.forms[0].Disp_Num.value;

	//入力チェック空かどうか
	if(!input_check_Space(User_Id)){
		errJsCode = "ERRJ0001";
		return false;
	}
	//入力チェック英数字かどうか（記号など不可）
	if(!input_check_Alphanumeric(User_Id)){
		errJsCode = "ERRJ0002";
		return false;
	}
	//入力桁が正常かどうか
	if(!input_check_length(User_Id,4,16)){
		errJsCode = "ERRJ0003";
		return false;
	}

//upd koyama password accept null
	if(modeStr == 'add' || User_Password != ''){
		//入力チェック空かどうか
		if(!input_check_Space(User_Password)){
			errJsCode = "ERRJ0004";
			return false;
		}
		//入力チェック英数字かどうか（記号など不可）
		if(!input_check_Alphanumeric(User_Password)){
			errJsCode = "ERRJ0005";
			return false;
		}
		//入力桁が正常かどうか
		if(!input_check_length(User_Password,4,16)){
			errJsCode = "ERRJ0006";
			return false;
		}
	}
	//入力チェック空かどうか
	if(!input_check_Space(Disp_Num)){
		errJsCode = "ERRJ0007";
		return false;
	}
	//入力チェック数字かどうか（正のみ）
	if(!input_check_Numeric(Disp_Num)){
		errJsCode = "ERRJ0008";
		return false;
	}
	//入力桁が正常かどうか
	if(!input_check_length(Disp_Num,1,2)){
		errJsCode = "ERRJ0009";
		return false;
	}
	//入力数値が範囲内かどうか
	if(!input_check_range(Disp_Num,15,99)){
		errJsCode = "ERRJ0010";
		return false;
	}

	if(modeStr == 'add'){
		//ユーザIDが既に存在しているか（画面上のリストと照合）
		var users = document.getElementById("userListBody");
		if(users){
			for(var i=0;i < users.children.length;i++){
				var targetIdTemp = users.children[i].children[1].innerHTML;
				if(User_Id == targetIdTemp || User_Id == "admin"){
					//存在している場合
					errJsCode = "ERRJ0011";
					return false;
				}
			}
		}
	}

	//パスワードと確認用パスワードが一致しているか
	if(User_Password != User_Conf ){
		errJsCode = "ERRJ0012";
		return false;
	}
	return true;
}
