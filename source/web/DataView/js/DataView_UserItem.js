var newCount = 1;
var errJsCode = '';

function typeCheck(typeText){
	var retVal=false;
	if(typeText.match('^(X|N|S?C?9+V?9*)$')){
		retVal = true;
	}
	return retVal;
}


//�a���o�^����ۂ̓��̓`�F�b�N
function input_check_item_conf() {
	//�e�[�u����
	tableName = document.getElementById("Table_Name").value;
	//�������`�F�b�N //�Ƃ肠����10����
	if(!input_check_length(tableName,1,10)){
		errJsCode = "ERRJ0066";
		return false;
	}
	//������`�F�b�N
	if(!tableName.match(/^[-a-zA-Z0-9]+$/)){
		errJsCode = "ERRJ0058";
		return false;
	}

	//�a���擾
	Japanese_Name = document.getElementById("Item_Japanese_Name").value;
	Japanese_Name = Japanese_Name.replace(/\s+/g, ""); //���p�X�y�[�X����������
	//�������`�F�b�N
	if(!input_check_length(Japanese_Name,1,20)){ //20�����܂łƂ���i�������\����W�����ȏ�ɂȂ�Ɨ����E�E�E�j
		errJsCode = "ERRJ0067";
		return false;
	}
	//�֎~�����`�F�b�N
	if(!input_check_symbol(Japanese_Name)){
		errJsCode = "ERRJ0068";
		return false;
	}

	//�J�n�ʒu�`�F�b�N
	if(!document.forms[(document.forms.length)-1].S_point.value.match('^[0-9]+$')){
		//������
		errJsCode = "ERRJ0062";
		return false;
	}
	if(document.forms[(document.forms.length)-1].S_point.value <= 0 || document.forms[(document.forms.length)-1].S_point.value > 4096){
		//�L���l�͈͓���
		errJsCode = "ERRJ0063";
		return false;
	}

	//�T�C�Y�`�F�b�N
	if(!document.forms[(document.forms.length)-1].Size.value.match('^[0-9]+$')){
		//������
		errJsCode = "ERRJ0069";
		return false;
	}
	if(document.forms[(document.forms.length)-1].Size.value <= 0 || document.forms[(document.forms.length)-1].S_point.value > 4096){
		//�L���l�͈͓���
		errJsCode = "ERRJ0070";
		return false;
	}

	//�^�`�F�b�N
	if(!typeCheck(document.forms[(document.forms.length)-1].Data_Type.value)){
		errJsCode = "ERRJ0061";
		return false;
	}

	return true;
}

function update(){
	regist('update');
}

function add(){
	regist('add');
}

//�X�V�{�^�����������Ƃ��̏���
function regist(modeStr) {
	//���b�Z�[�W������
	errJsCode = "";
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");
	////////////////////////////////////////////////////////////////////////////���̓`�F�b�N
	if(!input_check_item_conf()){
		//errJsCode��input_check_item_conf�ɂĐݒ肳��Ă���
//		//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
//		$(this).blur() ;	//�{�^������t�H�[�J�X���O��
//		//[$modal]���t�F�[�h�A�E�g������
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
	////////////////////////////////////////////////////////////////////////////�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./users_item.php');
	////////////////////////////////////////////////////////////////////////////�e�[�u�����擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','TableName');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Table_Name.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�g������ID
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Extension_Id');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Extension_Id.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////���ژ_����
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','JapaneseName');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Item_Japanese_Name.value.replace(/\s+/g, "")); //���p�X�y�[�X���������Ă���
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////���ژ_����
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].User_Id.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�D��\���t���O
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','NonDisp_Flg');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].NonDisp_Flg.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�J�n�ʒu�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','S_point');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].S_point.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�T�C�Y�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Size');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Size.value);
	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�^�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Data_Type');
	sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Data_Type.value);

	sendForm.appendChild( sendInp );
	////////////////////////////////////////////////////////////////////////////�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//�폜�N���b�N���̃C�x���g
function del(){

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//�����I������Ă��Ȃ���΃��b�Z�[�W��\��
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0060";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�������Ȃ���΍폜�����Ȃ�
	//hidden��input����擾
	Authority_flg = document.getElementById("Authority_flg").value;
	if(Authority_flg != "1" && (lclickSelElementsVar.children[8].innerHTML != $('#LoginId')[0].value)){
		errJsCode = "ERRJ0015";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�폜����Ă��鎞�̓��b�Z�[�W��\��
	if(lclickSelElementsVar.children[5].innerHTML == '�폜'){
		errJsCode = "ERRJ0018";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	var strMsg = '';
	strMsg += '�e�[�u����:['+ lclickSelElementsVar.children[2].innerHTML + '], ';
	strMsg += '���ږ�:['+ lclickSelElementsVar.children[3].innerHTML + '] ��' + "\n" ;
	strMsg += '�f�[�^�x�[�X���폜���܂��B��낵���ł����B';
	//�m�F��ʏo��
	if(!window.confirm(strMsg)){
		return ;
	}

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./users_item.php');

	//���[�UID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Extension_Id');
	sendInp.setAttribute('value',lclickSelElementsVar.children[1].innerHTML);
	sendForm.appendChild( sendInp );

	//�����t���O
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','del');
	sendForm.appendChild( sendInp );

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//�^����
function setTypeExplain(event){
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	var text = $(srcElem).attr('data-text');
	$(srcElem.parentNode).append('<div class="explain-tooltips">'+text+'</div>');
}
function delTypeExplain(event){
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	$(srcElem.parentNode).find(".explain-tooltips").remove();
}


//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function thisDocument(){
	var nb = $('.delButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , del);
	}

	addEvent("focus", document.forms[(document.forms.length)-1].Data_Type , function(event){ return setTypeExplain(event);});
	addEvent("blur", document.forms[(document.forms.length)-1].Data_Type , function(event){ return delTypeExplain(event);});
}

addEvent("load",window,thisDocument);
