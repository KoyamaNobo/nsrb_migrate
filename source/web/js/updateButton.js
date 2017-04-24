var updateButton = function (){
	//更新ボタン押下時
	var update = function(){
		//ユーザIDコンボボックスのdisabled属性を削除
		$('#user_id').removeAttr("disabled");
		//ユーザ名コンボボックスのdisabled属性を削除
		$('#user_name').removeAttr("disabled");
		//更新ボタンにdisabled属性を追加
		$('.button').attr('disabled', true);
		//コンボボックスの値をサーバに送信
		document.frm.submit();
	}
	var updateElem = $('#submit')[0];
	addEvent('click' , updateElem ,update );
}
addEvent('load',window,updateButton);