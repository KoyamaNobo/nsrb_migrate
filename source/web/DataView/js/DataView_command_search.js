//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function delButton(){
	var nb = $('#commandDeleteButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}
//�폜�N���b�N���̃C�x���g
function delClick(){

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//�������ʃ��X�g���I������Ă��Ȃ���΃G���[�\��
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0024";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//���O�C�����[�U�ȊO���쐬���������^�O�͍폜�����Ȃ�
	LoginId = document.getElementById("LoginId").value;
	if(lclickSelElementsVar.children[2].innerHTML != LoginId){
		errJsCode = "ERRJ0019";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�m�F��ʏo��
	if(!window.confirm('�����^�O��:['+ lclickSelElementsVar.children[1].innerHTML +'] ��DB���폜���܂��B��낵���ł����B')){
		return ;
	}
	
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_command_search.php');
	
	//�����^�OID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Command_Id');
	sendInp.setAttribute('value',lclickSelElementsVar.children[3].innerHTML);
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
addEvent('load',window,delButton);


//���փ{�^���������̃C�x���g�𒣂�t����
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}
//���փ{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
var next = function () {
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//�����I������Ă��Ȃ���΃��b�Z�[�W��\������return
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0024";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_disp.php');
	
	//�����^�OID
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','selectedCommandId');
	sendInp.setAttribute('value',lclickSelElementsVar.children[3].innerHTML);
	sendForm.appendChild( sendInp );
	
	//�v���O���������X�g�i���̉�ʖ��j
	var pgName = document.getElementById('pgName');
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',pgName.name);
	sendInp.setAttribute('value',pgName.value);
	sendForm.appendChild( sendInp );
	
	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);

