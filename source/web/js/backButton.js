var clickBack = function () {
	//戻るボタンのイベント
	var backButton = function (evt){
		let targArray   = new Array();
		let paramArray  = new Array();
		let splitArray   = new Array();
		let targUrl     = '';
		//以下正規表現のセット
		let regOpt      = new RegExp('opt=','');
		let regPm       = new RegExp('typ=pm','');
		let regJs       = new RegExp('typ=js','');
		let regLm       = new RegExp('typ=lm','');
		let regSm       = new RegExp('typ=sm','');
		let regPrevious = new RegExp('previous','');
		let regIniUppercase = new RegExp('[A-Z].*','');
		
		//exists opt
		if(location.href.match(regOpt)){
			targArray = location.href.split('&');
			
			for(let ii = 0;ii < targArray.length;i++){
				if(!targArray[ii].match(regOpt)){
					targUrl += targArray[ii] + '&';
				}
			}
			//最後の&を除去
			targUrl = targUrl.substr(0,(targUrl.length-1));
			document.location = targUrl;
		}
		
		//typ=pm
		if(location.href.match(regPm) || location.href.match(regSm)){
			history.back();
		}
		//typ=js
		//typ=lm
		if(location.href.match(regJs) || location.href.match(regLm)){
			//プロセスを落とす処理を追加
			if(!confirm('現在動いている処理を終了します。'+"\r\n"+'よろしいですか?')){
				return false;
			}
			$.ajax({
				type: "POST",
				url: "pk.php",
				data:{ infname:$('#infname')[0].value, outfname:$('#outfname')[0].value },
				success: function(msg,txt){
					history.back();
				},
				error:function(jqXHR,textStatus,errorThrown ){
					alert("error" + textStatus + ":" + errorThrown.message);
				}
			});
		}
		targArray = location.href.split('?');
		splitArray = targArray[0].split('/');
		if(!targArray[1]){
			splitArray.splice((splitArray.length - 1),1);
			document.location = splitArray.join('/') + '/';
		}
		if(splitArray[splitArray.length - 1].match(regIniUppercase)){
			let regDetail = new RegExp('Detail','');
			if(splitArray[splitArray.length - 1].match(regDetail)){
				//機能のメインに対応する画面以外
				let targFileName = splitArray[splitArray.length - 1];
				targFileName = targFileName.replace(regDetail,'');
				splitArray.splice((splitArray.length - 1),1);
				targUrl = splitArray.join('/');
				targUrl = targUrl + '/' + targFileName;
				document.location = targUrl;
			}else{
				splitArray.splice((splitArray.length - 1),1);
				targUrl = splitArray.join('/') + '/';
				document.location = targUrl;
			}
		}
	}


	let inputElements = $('input.button');
	for(let ii = 0; ii < inputElements.length ;ii++){
		//戻るボタンのイベント追加
		if($(inputElements[ii]).hasClass('backButton')){
			addEvent('click' , 
					inputElements[ii] , 
					function( evt ){
						return backButton( evt );
					}
			);
		}
		//rootMenuのイベント追加
		if($(inputElements[ii]).hasClass('rootMenu')){
			let menuForm = findParentByClassName(inputElements[ii],'foot');
			//rootMenu選択のキャンセルイベント
			$(menuForm).submit(
				function (evt){
					if($('#root')[0].value == ''){
						return false;
					}
					return true;
				}
			)
		}
	}
};
addEvent('load',window,clickBack);
