var cmbSelect = function (){
	//入力空のとき：選択項目をdisable,入力HTMLのとき要素の選択
	var cmbReset = function(ret) {
		if(ret.trim() == '1'){
			//formは一つしか作らない前提に
			for(let ii=0;ii < document.forms.frm.length;ii++){
				if(document.forms.frm[ii].name.match(/^user_/) == false){
					document.forms.frm[ii].disabled = true;
				}
			}
		}else if(ret.trim() == ''){
			//結果が正しくない時は戻す
			document.forms.frm.user_id.value  = document.forms.frm.buser_id.value;
			for(let ii=0;ii < document.forms.frm.length;ii++){
				if(document.forms.frm[ii].name.match(/^user_/) == false){
					document.forms.frm[ii].disabled = false;
				}
			}
		}else{
			//JSON形式で送られてきたデータをオブジェクトに戻す
			let retData = JSON.parse(ret);
			//user_idは選択によって変わっているので変更しない
			document.forms.frm.user_name.value          = retData['user_name'];
			document.forms.frm.font_color.value         = retData['font_color'];
			document.forms.frm.bg_color.value           = retData['bg_color'];
			document.forms.frm.font_size.value          = retData['font_size'];
			document.forms.frm.pg_id.value              = retData['pg_id'];
			document.forms.frm.reverse_bg_color.value   = retData['reverse_bg_color'];
			document.forms.frm.reverse_font_color.value = retData['reverse_font_color'];
			document.forms.frm.printer_id.value         = retData['print_id'];
			// for(let ii=0;ii < document.forms.frm.length;ii++){
			// 	document.forms.frm[ii].disabled = false;
			// }
			let viewMenu = $('.view_menu_item').find('input');
			for(let ii=0;ii < viewMenu.length;ii++){
				if(retData['pem' + viewMenu[ii].name] == '1'){
					viewMenu[ii].value = true;
					viewMenu[ii].checked = true;
				}else{
					viewMenu[ii].value = false;
					viewMenu[ii].checked = false;
				}
			}
		}
	}

	//ユーザIDコンボボックス選択時
	var userIdSelect = function(){
		//選択されたユーザIDの値
		let userId = $("select[name='user_id']");
		//ユーザIDコンボボックスのリスト
		let optionId = $('#user_id option');
		//選択されたユーザIDの値がリストの何番目か探す
		for(let count = 0; count < optionId.length; count++){
			if(optionId[count].text === userId.value){
				//ユーザ名コンボボックスのリスト
				var optionName = $('#user_name option');
				//ユーザIDに対応したユーザ名を選択
				optionName[count].selected = true;
				break;
			}
		}
		//リストの表示を変更すると動かなくなる箇所を発見->修正
		//ajaxで再取得しに行く
		//対象と送るデータの設定
		var targUrl = '';
		if(window.location.href.indexOf('?') < 0 ){
			targUrl = window.location.href;
		}else{
			targUrl = window.location.href.slice(0,window.location.href.indexOf('?'));
		}
		document.forms.frm.ajax_flg.value = 1;
		$.ajax({
			type: "POST",
			url: targUrl,
			data:{
				user_id:document.forms.frm.buser_id.value
				,chuser_id:document.forms.frm.user_id.value
				,ajax_flg:document.forms.frm.ajax_flg.value
			},
			success: function(ret){
				cmbReset( ret );
				document.forms.frm.ajax_flg.value = 0;
				sendFlag = false;
			},
			error: function(jqXHR,textStatus,errorThrown ){
				console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			}
		});
		cmbReset( '1' );
	}

	let selectElem = $('#user_id')[0];
	addEvent('change' , selectElem ,userIdSelect );
}

addEvent('load',window,cmbSelect);
