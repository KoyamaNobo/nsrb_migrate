var menuShortcut = function () {
	var menuSelect = function (evt){
		if ( navigator.userAgent.indexOf('MSIE') != -1 ){
			var e = window.event;
		}else{
			var e = evt;
		}
		if (!e){
			var e = window.event;
		}
		
		if(e.keyCode == 13){
			var targElements = $('.menuURL');
			var inputElements = $('#menuno');
			var replaceNumber = inputElements[0].value.replace(/[�`-�y��-���O-�X]/g, function(s) {
				return String.fromCharCode(s.charCodeAt(0) - 0xFEE0);
			});
			var regStr = new RegExp( "^0?" + replaceNumber + " ", "g");
			var regUSStr = new RegExp( "^(US|us)$", "g");
			var regBACKStr = new RegExp( "^(99)$", "g");
			var regDEStr = new RegExp( "^(DE|de)$", "g");
			var regRPStr = new RegExp( "^(RP|rp)$", "g");  //root permission
			var regDVStr = new RegExp( "^(DV|dv)$", "g");  //Data View
			var regFEStr = new RegExp( "^(FE|fe)$", "g");  //File Explorer
			
			//UserSetting�ɑJ��
			if(inputElements[0].value.match(regUSStr)){
				document.location = './UserMaster.php';
			}
			
			//DataExchange�ɑJ��
			if(inputElements[0].value.match(regDEStr)){
				document.location = './DataExchange.php';
			}
			
			//FileExplorer.php�ɑJ��
			if(inputElements[0].value.match(regFEStr)){
				document.location = './FileExplorer.php';
			}
			
			//DataView�ɑJ��
			if(inputElements[0].value.match(regDVStr)){
				window.open('./DataView/DataView_logout.php');
			}
			//�B��PG���s���ɑJ��
			if(inputElements[0].value.match(regRPStr)){
//				document.location = './root.php';
				window.open('./root.php');
			}
			
			//�X�X���͎��i�߂�{�^���N���b�N�j
			if(inputElements[0].value.match(regBACKStr)){
				if ($('.backButton')[0]) {
					$('.backButton')[0].click();
				}
			}
			
			for(var i=0;i < targElements.length;i++){
				if(targElements[i].innerHTML.match(regStr)){
					document.location = targElements[i].getAttribute('href');
				}
			}
			return false;
		}
	}


	var inputElements = $('#menuno');
	for(var i = 0; i < inputElements.length ;i++){
		//�߂�{�^���̃C�x���g�ǉ�
		addEvent('keydown' , 
				inputElements[i] , 
				function( evt ){
					return menuSelect( evt );
				}
		);
		//id�w��Ȃ̂łЂƂ��������͂��B
		//�y�[�W���[�h�̃^�C�~���O�Œ��g�������
		$('#menuno')[0].value = "";
		$('#menuno').focus();
	}
};
addEvent('load',window,menuShortcut);
