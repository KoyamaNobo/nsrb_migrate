//setTimeout�p�̕ϐ�
var TimeoutID;
//buzzer�p�̕ϐ�(�Ȃ炵���痧�Ă�)
var BuzzerFlg=0;
//buzzer�p��timeOutID
var BuzzerTimeoutID;
//��x�������͂�����A���Ă���܂ő���Ȃ�
var sendFlag = false;
//Error������̔z��
var ErrorStrArray = new Array();
//�G�X�P�[�v�{�^���������t���O�i������true�j
var escKeyFlg = false;
//�^�C���A�E�g���� 16�ȉ���FireFox�͓����Ȃ�
var TimeoutTime = 16;
//�^�C���A�E�g���� 16�ȉ���FireFox�͓����Ȃ�
var ReplaceModeFlg = 0;
//history.back()����x�ȏサ�Ȃ�����
var historyBackFlg = false;

//����I�ɉ�ʂ��Ƃ��Ă���
function fncChangeScreen() {
	var outfname = $('#outfname')[0];
	var infname  = $('#infname')[0];
	var pid      = $('#pid')[0];
	//menu���̏ꍇinfname���Ȃ�
	if( infname == void 0){
		return ;
	}
	if(sendFlag == false){
		$.ajax({
			type: "POST",
			url: "getOut.php",
			data:{ infname:infname.value, outfname:outfname.value, pid:pid.value },
			success: function(msg,txt){
				screenReplace(msg);
			},
			error: function(jqXHR,textStatus,errorThrown ){
				if(errorThrown){
					alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					reStart();
				}
			}
		});
		outfname = null;
		infname  = null;
		pid = null;
	}else{
		outfname = null;
		infname  = null;
		pid = null;
		reStart();
	}
};


//���͂��񂽁[���p�����[�^�𑗐M
function nextInput(event){
	//���͕ϊ�
	//author : koyama 20170202
	function change_input_send(event){
		let inputValue='';
		let statusValue='';
		//���̓`�F�b�N
		if(inpCheck(event)){
			//���̓t�H�[�}�b�g�i�����Ή��j
			inputValue = inpFormat(event);
		}else{
			return false;
		}
		//���͒l�̐ݒ�
		switch (event.key) {
			case 'Enter' :
				if(event.shiftKey == true){
					statusValue = 'f';
					break;
				}
				statusValue = 'h';
				break;
			case 'Tab' :
				statusValue = 's';
				break;
			case 'Backspace' :
				if(targElem.value != ""){
					return true;
				}
				inputValue = '';
				statusValue = 'b';
				break;
			case 'F10':
			case 'Alt':
				if(event.shiftKey == true){
					statusValue = '10';
				}else{
					statusValue = 'a';
				}
				break;
			case 'F1' :
			case 'F2' :
			case 'F3' :
			case 'F4' :
			case 'F5' :
			case 'F6' :
			case 'F7' :
			case 'F8' :
			case 'F9' :
				if(event.key == 'F4' && event.shiftKey == true){
					statusValue = '14';
					break;
				}
				if(event.key == 'F8' && event.shiftKey == true){
					statusValue = 'c1';
					break;
				}
				if(event.key == 'F9' && event.shiftKey == true){
					statusValue = 'c2';
					break;
				}
				statusValue = event.key.substr(1);
				break;
				case 'Process' :
					//2�o�C�g����
					replaceModeExec(event);
					// �S�p���p�L�[�������ꂽ��Ƃ肠�������M�͂��Ȃ�
					return true;
			default:
				return true;
		}
		//sendFlag��Timeout�̏���
		preProcess();
		$.ajax({
			type: "POST",
			url: "param.php",
			data:{ value:inputValue + '`'+ statusValue ,infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
			success: function(msg){
				screenReplace( msg );
				sendFlag = false;
			},
			error: function(jqXHR,textStatus,errorThrown ){
				alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
				console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			}
		});
		//�r����true�Ƃ��Ĕ����Ȃ�����false
		//�����������Ŏ~�߂�Ƃ���false��Ԃ�
		return false;
	}

	//�㏑�����[�h�̎�,���̈ꕶ��������
	function replaceModeExec(event){
		//event.target.selectionStart ->���ꂪ�L�����b�g�̈ʒu
		event.target.value = event.target.value.substr(0,event.target.selectionStart);
	}
	let targElem = evtToElement(event);
	let outfname = $('#outfname');
	let infname  = $('#infname');
	let pid  = $('#pid');
	let input = '';

	//menu���̏ꍇinfname���Ȃ�
	if( infname == void 0){
		return ;
	}
	if(sendFlag == false){
		//�G���[���b�Z�[�W���\�����@����
		//�W���u�I�����̓L�[���얳��
		let tmp = $("#status1").html();
		tmp = jQuery.trim(tmp.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g,''));
		if(tmp.length > 0 && $('#parentStatus')[0].value == "end" ){
			return false;
		}
		//CTL+ANY�L�[�̏���(��ʐؗ��֌W)
		if(screenSwitch(event)){
			//����L�[�̏ꍇ�͂���ȏ㏈�����Ȃ�
			event.stopImmediatePropagation();
			// return false;
		}

		//CTL+ANY�L�[�̏���
		if(funcSpecialKey(event)){
			//����L�[�̏ꍇ�͂���ȏ㏈�����Ȃ�
			event.stopImmediatePropagation();
			// return false;
		}
		//���͂ɉ�������Ή��L�֐��ŁA�����Ȃ����true�ŋA���Ă���
		let retVal;
		retVal = change_input_send(event);
		if(retVal == false){
			//���͂��������Ƃ��̓o�u�����O�L�����Z��
			// if(event.defaultPrevented != true){
			// 	event.preventDefault();
			// }
			// event.stopImmediatePropagation();
			// event.stopPropagation();
		}
		// return retVal;
	}
};

//�t�H�[�J�X��input�ł͂Ȃ��Ƃ��̃L�[�C�x���g
var anyKeyEvent = function(evt){
	var parentStatus  = $('#parentStatus');

	//�G���[���b�Z�[�W���\�����@����
	//�W���u�I�����̓L�[���얳��
	let tmp = $("#status1").html();
	tmp = jQuery.trim(tmp.replace(/<("[^"]*"|'[^']*'|[^'">])*>/g,''));
	if(tmp.length > 0  && parentStatus[0].value == "end" ){
		return false;
	}

	if(sendFlag != false){
		return true;
	}
	//F5 + ctrl
	if(evt.keyCode >= 112 && evt.keyCode <= 123){
		let targElem = evtToElement(evt);
		if(evt.ctrlKey == true){
			//�����input����Ȃ��Ƃ��ɓ����H
			//CTL+ANY�L�[�̏���(��ʐؗ��֌W)
			if(screenSwitch(evt)){
				//����L�[�̏ꍇ�͂���ȏ㏈�����Ȃ�
				return false;
			}

			let menuCmb = $('#root')[0];
			var forcusElem = (document.activeElement || window.getSelection().focusNode);
			if(forcusElem == menuCmb || forcusElem.tagName.toLowerCase() == 'input'){
				forcusElem=null;
				menuCmb   =null;
				return false;
			}

			//CTL+ANY�L�[�̏���
			if(funcSpecialKey(evt)){
				//����L�[�̏ꍇ�͂���ȏ㏈�����Ȃ�
				return false;
			}

			sendFlag = true;
			//menu���̏ꍇinfname���Ȃ�
			if($('#infname').length != 0){
				var outfname = $('#outfname');
				var infname  = $('#infname');
				var pid      = $('#pid');
				//sendFlag��Timeout�̏���
				preProcess();
				$.ajax({
					type: "POST",
					url: "param.php",
					data:{ value:'F' + (evt.keyCode - 111),infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
					success: function(msg){
						screenReplace( msg );
						sendFlag = false;
					},
					error: function(jqXHR,textStatus,errorThrown ){
						alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
						console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
					}
				});
				outfname=null;
				infname =null;
				pid     =null;
			}
		}
		return false;
	}
}

//�L�[�A�b�v���̏����inextInput�j
var setInputColor = function(evt){
	if($('input.nextinput:focus').length > 0){
		if( $('input.nextinput:focus')[0].value != ''){
			$('input.nextinput:focus').css('background-color',document.getElementById("sbgcolor").value);
		}else{
			$('input.nextinput:focus').css('background-color','');
		}
	}
}

//�L�[�A�b�v���̏����i�L�[�_�E���ł͑Ή��s�j
var escCancel = function(evt){
	//���̓`�F�b�N
	if(!inpCheck(evt)){
		return false;
	}

	if (evt.keyCode == 27 && sendFlag == false) { // Esc
		var targElem = evtToElement(evt);
		var outfname = $('#outfname');
		var infname  = $('#infname');
		var pid  = $('#pid');
		try{
		escKeyFlg = true;
		//sendFlag��Timeout�̏���
		preProcess();
		$.ajax({
			type: "POST",
			url: "param.php",
			data:{ value:'`esc' ,infname:infname[0].value, outfname:outfname[0].value, pid:pid[0].value },
			success: function(msg){
				screenReplace( msg );
				sendFlag = false;
			},
			error: function(jqXHR,textStatus,errorThrown ){
				alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
				console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			}
		});
		}catch(e){
			escKeyFlg = false;
			sendFlag = false;
		}
		return false;
	}
}

var setBlinkToElement = function(changeScreen){
	targetArray = $(changeScreen).find('.blink');
	var ii = 0;
	if(typeof(blinkVisibleFlg) == "undefined" || blinkVisibleFlg == false){
		for(ii=0;ii < targetArray.length;ii++ ){
			targetArray[ii].style.visibility = "visible";
		}
	}else{
		for(ii=0;ii < targetArray.length;ii++ ){
			targetArray[ii].style.visibility = "hidden";
		}
	}
	targetArray = null;
	return changeScreen;
}


//screen�̃G�������g�����ɕύX���Ă���
//input��jQuery�I�u�W�F�N�g��
var replaceScreenElement = function(targScreen,changeScreen){

	var stopFlg = -99;        //
	var remElemArr =new Array();
	//���M�ς݂�input�폜?
	remElemArr = removeSendedInput();
	//�v���Z�X�I�����̖߂�Ή��E�X�e�[�^�X�o�[�Ή��i�W���u�̎��s��Ԃ����݁j

	changeScreen = setBlinkToElement(changeScreen);

//	//��ʂ̗v�f�����Ԃɔ�r�B�Ⴄ�v�f�͕ύX��������
	//�ǂ��炩�ɃC���v�b�g�����݂��Ȃ���Ώ���������
	if(targScreen.find('input').length > 0  && changeScreen.find('input').length > 0){
		//�Ō�̃C���v�b�g���r�B���O���ꏏ�Ȃ�Ƃ肠�����AOK
		//���s�t�@�C�����ς���Ă��������O�ł�����x���������Ƃ��͏�������
		(function(){
			var origCount = 0;
			var changCount = 0;
			for(origCount =0;origCount < targScreen[0].children.length ;origCount++ ){
				var curElem = targScreen[0].children[origCount];
				//line�N���X�������Ă�����s�̃e�L�X�g�n
				if($(curElem).hasClass('line')){
					//�S������class�������Ă�����̂ō����ւ�
					//�s�̃��[�v
					for(changCount = 0;changCount < changeScreen[0].children.length ;changCount++){
						if(curElem.getAttribute('class') == changeScreen[0].children[changCount].getAttribute('class')){
							//input���܂܂��ꍇ�v�f������ɕ������ĕϊ�
							if($(curElem).find('input').length > 0){
								//input�������Ă���Ƃ���ȊO���폜
								//�P�s���̗v�f�̏���
								for(var i=0;i < curElem.children.length;i++){
									//��������input���܂܂�Ȃ��Ƃ�����폜
									if(curElem.children[i].tagName.toLowerCase() != 'input'){
										curElem.removeChild(curElem.children[i]);
										//remove�����Ɣz��ԍ����ς��̂�
										//������jQuery�I��remove���Ȃ����
										i--;
									}else{
										//input�̍��ڂ͎��̉�ʊ܂܂�Ă��Ȃ��Ƃ������폜
										//input�͉�ʂɈ�ɂȂ�悤�ɂ��Ȃ���΂����Ȃ�       �݂��� lastchild�𐳂����ʒu�ɒu���΂����H
										var nextViewExistFlg = 0;
										for(var j=0;j < changeScreen[0].children[changCount].children.length;j++){
											var curCoulumn = curElem.children[i].className.match(/\s+f[0-9]+/i);
											var changeCoulumn = changeScreen[0].children[changCount].children[j].className.match(/\s+f[0-9]+/i);

											//���g�ȊO��input���������ɔ�΂�
											if(curCoulumn == null || changeCoulumn == null){
												break;
											}

											//input�����ւ��邩�ǂ���
											if((curElem.children[i].name == changeScreen[0].children[changCount].children[j].name )
											&& (curCoulumn[0] == changeCoulumn[0])
											&& remElemArr.indexOf(curElem.children[i].name) == -1){
												//curCoulumn[0] == changeCoulumn[0]�ŉ��ɓ������O���������̑Ή�
												nextViewExistFlg = 1;

												//�G���[�Ŏ~�܂��Ă��鎞��STOP��Input������̂�
												//��x�ȏ�o���Ă�����Ăяo���Ȃ�
												if(curElem.children[i].name == "STOP" && BuzzerFlg != 1){
													stopFlg = 1;
												}
											}
										}
										if(nextViewExistFlg == 0)	{
											curElem.removeChild(curElem.children[i]);
											//remove�����Ɣz��ԍ����ς��̂�
											i--;
										}
									}
								}
								//input�������Ă��Ȃ��Ƃ����ǉ�
								for(var j=0;j < changeScreen[0].children[changCount].children.length;j++){
									//input���܂܂�Ȃ��Ƃ����ǉ�
									if(changeScreen[0].children[changCount].children[j].tagName.toLowerCase() != 'input'){
										curElem.appendChild(changeScreen[0].children[changCount].children[j]);
										//append�����Ɣz��ԍ����ς��̂�
										j--;
									}else{
										//input�̍��ڂ͑O�̉�ʊ܂܂�Ă��Ȃ��Ƃ������ǉ�
										var nextViewExistFlg = 0;
										for(var i=0;i < curElem.children.length;i++){
											if(changeScreen[0].children[changCount].children[j].name == curElem.children[i].name){
												nextViewExistFlg = 1;
											}
										}
										if(nextViewExistFlg == 0){
											if(changeScreen[0].children[changCount].children[j].name != "STOP"){
												BuzzerFlg=0;
											}
											curElem.appendChild(changeScreen[0].children[changCount].children[j]);
											//append�����Ɣz��ԍ����ς��̂�
											j--;
										}
									}
								}
							}else{
								//input���Ȃ��Ƃ��Ƃ͒P���ɍ����ւ�
								curElem.innerHTML = changeScreen[0].children[changCount].innerHTML;
							}
							//�ύX����element�͍폜���Ă���
							//remove
							changeScreen[0].removeChild(changeScreen[0].children[changCount]);
							break;
						}
					}

				}else{
					//�r���͈���������Ă��邱�Ƃ��l���Ĉ�xremove
					targScreen[0].removeChild(curElem);
					//remove�����Ɣz��ԍ����ς��̂�
					origCount--;
				}
				curElem = null;
			}
			//�r������������
			var i = 0;
			while(i < changeScreen[0].children.length){
				if(changeScreen[0].children[i].tagName.toLowerCase() == 'div'){
					targScreen[0].appendChild(changeScreen[0].children[i]);
					//append�����Ɨv�f�������Ă����̂�
					i--;
				}
				i++;
			}
		})();
	}else{
		targScreen[0].innerHTML = changeScreen[0].innerHTML;
	}

	//��ʂ����ւ������ʐF�č\��
	//buzzuer���O��
	userSetting();

	//�����l��-99�Ƃ��Ă���̂�
	if(stopFlg > -99 && sendFlag == false){
		(function (){
			buzzerStart("");
			sendFlag = true;
		})();
		//�������珉����
		stopFlg=-99;
	}

	//���[�_��
	if($('#err-buz').length > 0){
		var soundLength = document.getElementById('err-buz').value;
		buzzerStart(soundLength);
	}
	//���̂�
	if($('#info-buz').length > 0){
		var soundLength = document.getElementById('info-buz').value;
		buzzerStart(soundLength);
	}
}

//�v�f�����ւ���
function screenReplace(resultTxt){
	var screenObj = $(".screen");
	var changeObj = document.createElement('div');
	changeObj.innerHTML = resultTxt;
//	var changeObj = $(resultTxt);

	changeObj = changeStatus(changeObj);
	if(changeObj == null){
		return 1;
	}
	//�擾�s�̂��ꂩ��
	changeObj = setLineIndex(changeObj);
	replaceScreenElement(screenObj ,$(changeObj));

//	$(changeObj).('*').remove();
	lineTxt   = null;
	screenObj = null;
	changeObj = null;
	//�C�x���g�̓\�蒼��
	reStart();

	return 0;
}

//���͎��̃G���^�[���L�����Z��
var enterCancel = function (evt){
	switch(evt.Key){
	case 'F10':
	case 'Enter' :
		return false;
		break;
	default :

	}
}

var AnyKeyDownStopBuzzer = function(){
	//sendFlag��Timeout�̏���
	preProcess();
	$.ajax({
		type: "POST",
		url: "param.php",
		data:{ value: '`h' ,infname:$('#infname')[0].value, outfname:$('#outfname')[0].value, pid:$('#pid')[0].value },
			success: function(msg){
			screenReplace( msg );
			sendFlag = false;
			stopFlg = -99;
		},
		error: function(jqXHR,textStatus,errorThrown ){
			alert("BackGround connect Error" + textStatus + ":" + errorThrown.message);
			console.log("BackGround connect Error" + textStatus + ":" + errorThrown.message);
		}
	});
	BuzzerFlg = 0
	clearTimeout(BuzzerTimeoutID);
}


var buzzerUntilAnyKeyDown = function(soundLength){
	BuzzerFlg = BuzzerFlg + 1
	if(BuzzerFlg <= soundLength){
	  // base64�������\��t��(���̒����������ŃR���g���[������K�v�L��H
	  var base64 = "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"
	  // datauri scheme �`���ɂ��� Audio �I�u�W�F�N�g�𐶐����܂�
	  var sound = new Audio("data:audio/wav;base64," + base64);
	  // ����炵�܂�
	  sound.play();
/*
		var msg = '�G���[���������܂����B\n';
		msg  +=  '�Ǘ��҂ɘA�����Ă��������B\n';
		msg  +=  soundLength + '';
		alert(msg);
		BuzzerFlg = 1;
*/
	}else{

	}
	if(!escKeyFlg){
		clearTimeout(BuzzerTimeoutID);
		BuzzerTimeoutID=setTimeout('buzzerUntilAnyKeyDown()',50);
	}

}

//beep���̏���
var buzzerStart = function(soundLength){
	var elemTargs = $('.screen input:last');
	//escape�̃t���O���肹���Ƃ��ĉ������܂�
	escKeyFlg = false;
	addEvent('keydown' ,
			elemTargs[0],
			function evtAnyKeyDown( event ){
				return AnyKeyDownStopBuzzer( event );
			}
	);
	elemTargs = null;
	buzzerUntilAnyKeyDown(soundLength);

}
//TODO:: ���͒l���o�b�t�@����d�g��
//20161117 add koyama
var InputBuffer = function(event){
	var targElem = evtToElement(event);
//	if(targElem.tagName.toLowerCase().match('intput')){
		// console.log('======'+'not input'+'==='+event.key+'=='+event.type+'==='+ targElem.tagName.toLowerCase() +'===========');
		// console.timeStamp('not input');
		// console.trace();
//	}
}

var inputAddElement = function (){
	var inputElements = $('.screen input');
	for(var i = 0; i < inputElements.length ;i++){
		//����tab stop�ֈړ��̃C�x���g�ǉ�
		if($(inputElements[i]).hasClass('nextinput')){
			addEvent('keydown' ,
					inputElements[i] ,
					function evtNextInput( evt ){
						return nextInput( evt );
					}
			);
			addEvent('keyup' ,
				inputElements[i] ,
				function evtSetInputColor( evt ){
					return setInputColor( evt );
				}
			);
		}
		addEvent('keyup' ,
		inputElements[i] ,
		function evtSetReplaceMode( evt ){
			if(evt.keyCode == 45 && evt.code == "Insert"){
				ReplaceModeFlg = (ReplaceModeFlg + 1) % 2;
			}
			return false;
		}
		);
		addEvent('keypress' ,
				inputElements[i] ,
				function evtEnterCancel( evt ){
					return enterCancel( evt );
				}
		);
	}
	inputElements = null;
	addEvent('keydown' ,
			window.document,
			function evtAnyKeyEvent( evt ){
				return anyKeyEvent( evt );
			}
	)
	addEvent('keyup' ,
			window.document,
			function evtEscCancel( evt ){
				return escCancel( evt );
			}
	)
	//TODO:: ���͒l���o�b�t�@����d�g�݂���� ->�o�����炻��������͂��s���悤�Ɏd�l�ύX
	//20161117 add koyama
	var screenElements = $('.screen');
	addEvent('keypress' ,screenElements[0], InputBuffer )
	screenElements = null;

	//�t�@���N�V�����L�[�̋@�\�͎g�킹�Ȃ��悤��
	$(window).keydown(function (event){
		//F5 + ctrl
		if((event.key.match(/F[0-9]{1,2}/)) || event.key == 'Alt' ){
			return false;
		}
		return true;
	});
}

//�ăX�^�[�g�p
function reStart(){
//	sendFlag = false;
	//�C�x���g�̓\�蒼��
	inputAddElement();
	if(document.activeElement.id != "skSelect"){
		$('input.nextinput:last').focus();
	}

	clearTimeout(TimeoutID);
	TimeoutID = setTimeout('fncChangeScreen()',TimeoutTime);
}

//���M�ς݂�input�폜
//author:koyama
var removeSendedInput = function(){
	var retVal = new Array();
	//���͏I��������A���̃G�������g���폜
	if(sendFlag == true){
		var i=0;
		while(0 < $('.screen input').length){
			retVal[i] = $('.screen input:first')[0].name;
			i++;
			//TODO: �����̓��������[�N�H
			$('.screen input:first').remove();
		}
	}

	return retVal;
}

//��ʂɍ��܂łɉ��s�Ƃ��������Đݒ�
//��ʂ�responce�ɐݒ肪�Ȃ���Ή��������I��
//author:koyama
//date  :20151005
function setLineIndex(resObj){
	var ii = 0;

	if($(resObj).find('#line_index').length > 0){
		//�l�͈�ڂ�window�̑S�Ă�element�ɓ���鏈���ɂ��Ă���
		$('#line_index')[0].value = $(resObj).find('#line_index')[0].value;
		$(resObj).find('#line_index').remove();
	}
	return resObj;
}

//changeScreen�̒�����X�e�[�^�X�Ɋ֘A������̂��擾���ĉ�ʂɕ\��
//�\����͕s�v�ɂȂ邽�ߏ���
function changeStatus(changeScreen){
	//changeScreen�̒�����error�T���ĕ\��
	if($(changeScreen).find('.error').length > 0){
		for(var ii=0;ii < $(changeScreen).find('.error').length ;ii++ ){
			//���������񂪌�����Ȃ�������
			if(ErrorStrArray.length == 0 || ($(changeScreen).find('.error').length > 0 && $.inArray($(changeScreen).find('.error')[ii].value,ErrorStrArray) == -1)){
				escKeyFlg = false;
				//�X�e�[�^�X�o�[�Ή�
				var status1 = $("#status1")[0];
				status1.innerHTML = "<span>" + $(changeScreen).find('.error')[0].value + "</span>";
				ErrorStrArray.push($(changeScreen).find('.error')[ii].value);
				status1 = null;
			}
		}
		//���Ƃɉe��������ڂ��̂ō폜���Ă���
		if($(changeScreen).find('#status1Get')[0]){
//			var elemParent = $(changeScreen).find('#status1Get')[0].parentNode;
			$(changeScreen).find('#status1Get').remove();
		}
	}

	//�v���Z�X�I�����̖߂�Ή�
	//parentStatusGet�����݂��Ă���ꍇparentStatus�ɑ������
	//changeScreen�̒�����parentStatusGet������
	if($(changeScreen).find('.parentStatusGet').length > 0){
		$("#parentStatus").val($(changeScreen).find('.parentStatusGet')[0].value);
		$(changeScreen).find('.parentStatusGet').remove();
	}

	//pid���擾�o���Ȃ��Ă��I�������Ƃ������ƂȂ̂ŁA�߂�
	if($('#pid') == void 0 && historyBackFlg == false){
		// location.href = document.referrer;
		historyBackFlg = true;
		history.back();
		return null;
	}
	//�v���Z�X�I�����̖߂�Ή�
	//��U��ʂɏ����o�����������
	//�������G���[���b�Z�[�W������Ƃ���Esc�L�[�������̂ݖ߂�
	if($('#parentStatus').length > 0 && $('#parentStatus')[0].value == "end"){
		if(ErrorStrArray.length == 0 && historyBackFlg == false){
			// location.href = document.referrer;
			historyBackFlg = true;
			history.back();
			return null;
		}else{
			if(escKeyFlg == true && historyBackFlg == false){
				escKeyFlg = false;
				// location.href = document.referrer;
				historyBackFlg = true;
				history.back();
				return null;
			}
		}
	}

	return changeScreen;
}

function preProcess(){
	sendFlag = true;
	clearTimeout(TimeoutID);
}

//load���ɍŏ��ɂ�����e��
var loadTime = function () {
	inputAddElement();

	TimeoutID = setTimeout('fncChangeScreen()',TimeoutTime);
};

addEvent('load',window,loadTime);
