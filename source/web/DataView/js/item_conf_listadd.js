var getItemAjax = function (event) {
	var url = "item_conf_listadd_ajax.php";
	var request = createXmlHttpRequest();
	var param = "num=";
	var srcURL = '';
	var paramElem = document.getElementById('page_num');
	var srcElem;
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	srcURL = encodeURI(location.pathname.split('/')[location.pathname.split('/').length - 1]);
	//���݂�URL�ɂ���ă^�[�Q�b�g��URL��ύX
	if(srcURL.match(/users_item.php/i)){
		url = 'user_item_listadd_ajax.php';
	}
	//90%�ȏ�X�N���[��������
	if(request && (srcElem.scrollTop / (srcElem.scrollHeight - srcElem.clientHeight) * 100) > 90 && paramElem.className != "off"){
		param += paramElem.value;
		request.open("GET", url + '?' + param, true);
		request.send("");
		paramElem.setAttribute('class','off');
		request.onreadystatechange = function() {
			if(request.readyState == 4 && request.status == 200) {
				paramElem.setAttribute('class','on');
				if(request.responseText != ""){
					paramElem.value = String(parseInt(paramElem.value) + 1);
				}
				setMoreItemToList(srcElem,request.responseText);
			}else{
//					alert('�ǉ����ڂ��擾�ł��܂���ł���');
			}
		}
	}

	function setMoreItemToList (targElem,responseText){
		var liItem;  //�����p�̐e�G�������g�쐬
		var flgOdd=0;
		liItem = document.createElement('div');
		liItem.innerHTML = responseText;
		while( targElem.lastChild.nodeName.toLowerCase() == '#text' ){
			targElem.removeChild( targElem.lastChild );
		}
		if(targElem.lastChild.getAttribute('class').match(/odd/i)){
			flgOdd = 1;
		}

		for(var ii=0;ii < liItem.children.length;ii++ ){
			var cloneElem = liItem.children[ii].cloneNode(true)
			addEvent("click", cloneElem , setClickElems);
			targElem.appendChild(cloneElem);
			//�オ����������킩��Ȃ��̂�class�͕t������
			if(((ii + flgOdd) % 2)){
				targElem.lastChild.setAttribute('class','listElem');
			}else{
				targElem.lastChild.setAttribute('class','listElem odd');
			}
		}
	}
}

var getMoreItemAtScroll = function () {
	var scrollElem = $('.lcontents .leftList');
	var ii = 0;

	for(;ii< scrollElem.length;ii++){
		addEvent("scroll", scrollElem[ii] , function(event) { return getItemAjax(event);});
	}
}

addEvent('load',window,getMoreItemAtScroll);
