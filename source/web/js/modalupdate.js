var modalUpdate = function () {
	var modalUpdateTime;
	var modalUpdateTimeoutTime = '1000';

	function changeModalList(responceText){
			var targElement = $('#modalDetail')[0];
			var changeObj = document.createElement('div');
			changeObj.innerHTML = responceText;
			$('#modal-open')[0].value = $('#modal-open')[0].value.replace(/^[0-9]+/,changeObj.children.length);
			//子要素を全て削除
			while (targElement.firstChild) targElement.removeChild(targElement.firstChild);
			//appendすると要素が移動する
			for(var ii=0;changeObj.children.length != 0;ii++){
				targElement.appendChild(changeObj.firstChild);
			}
	}
	var loadModalList = function(){
			$.ajax({
			type: "POST",
			url: "modalList.php",
			data:{ infname:'test' },
			success: function(msg,txt){
				changeModalList(msg);
			},
			error: function(jqXHR,textStatus,errorThrown ){
				if(errorThrown){
					console.log("modal connect Error"+":" + errorThrown.message);
				}
			}
		});
		//input or a 以外にforcusが当たっているときはfocusをmenunoに戻す
		if(['input','a','select'].indexOf(document.activeElement.tagName.toLowerCase()) < 0){
			$('#menuno').focus();
		}
		modalUpdateTime = setTimeout(loadModalList, modalUpdateTimeoutTime);
	}
	modalUpdateTime = setTimeout(loadModalList, modalUpdateTimeoutTime);
};
addEvent('load',window,modalUpdate);
