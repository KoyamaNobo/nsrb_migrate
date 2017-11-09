var StatusUpdate = function () {
	var StatusUpdateTime;
	var StatusUpdateTimeoutTime = '1000';

	function changeStatusList(responceText){
			let targElement = $('.screen')[0];
			let changeObj = document.createElement('div');
			changeObj.innerHTML = responceText;
			//子要素を全て削除
			while (targElement.firstChild) targElement.removeChild(targElement.firstChild);
			//appendすると要素が移動する
			for(var ii=0;changeObj.children.length != 0;ii++){
				if(changeObj.firstChild.id == 'send_count'){
					//行数のセット
					let replaced = ''
					replaced = document.getElementById("listnum").value.replace(/\d+/g,changeObj.firstChild.value);
					if(replaced != ''){
						document.getElementById("listnum").value = replaced
					}
				}
				if(changeObj.firstChild.id == 'send_max'){
					//行数のセット
					document.getElementById("listmax").value = changeObj.firstChild.value;
					if(document.getElementById("page_num").value > document.getElementById("listmax").value){
						document.getElementById("page_num").value = document.getElementById("listmax").value;
						clearTimeout(StatusUpdateTime);
						loadStatusList();
					}
				}
				targElement.appendChild(changeObj.firstChild);
			}
			userSetting();
	}
	function loadStatusList(){
			let stmode = document.getElementById("current_mode").value;
			let stpage = document.getElementById("page_num").value;
			$.ajax({
			type: "POST",
			url: "status_list.php",
			data:{ status_mode:stmode,page_num:stpage },
			success: function(msg,txt){
				changeStatusList(msg);
				StatusUpdateTime = setTimeout(loadStatusList, StatusUpdateTimeoutTime);
			},
			error: function(jqXHR,textStatus,errorThrown ){
				if(errorThrown){
					console.log("Status connect Error"+":" + errorThrown.message);
				}
				StatusUpdateTime = setTimeout(loadStatusList, StatusUpdateTimeoutTime);
			}
		});
		//input or a 以外にforcusが当たっているときはfocusをmenunoに戻す
		if(['input','a','select'].indexOf(document.activeElement.tagName.toLowerCase()) < 0){
			$('#menuno').focus();
		}
	}
	StatusUpdateTime = setTimeout(loadStatusList, StatusUpdateTimeoutTime);
	//ページ変更の処理
	var statusPageChange = function(event){
			let regLt       = new RegExp('<','');
			let regGt       = new RegExp('>','');
			let eventElem = evtToElement(event);
			let pageNumElem = document.getElementById('page_num');
			if(eventElem.value.match(regLt)){
				//減らすのは1以上の範囲
				if(+pageNumElem.value > 1){
					pageNumElem.value = (+pageNumElem.value - 1).toString();
				}
			}else if (eventElem.value.match(regGt)) {
				//減らすのはMax以下の範囲
				if(+pageNumElem.value < document.getElementById("listmax").value){
					pageNumElem.value = (+pageNumElem.value + 1).toString();
				}
			}
			clearTimeout(StatusUpdateTime);
			loadStatusList();
	}
	//ページ変更の処理イベント貼り付け
	let targElement = document.getElementsByClassName("page_change");
	for(var ii=0;ii < targElement.length;ii++){
		addEvent('click',targElement[ii],function(event){ return statusPageChange(event)});
	}
};
addEvent('load',window,StatusUpdate);
