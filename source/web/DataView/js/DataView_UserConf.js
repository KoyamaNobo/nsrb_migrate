var lclickSelElementsVar;
var errJsCode;

//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function delButton(){
	var nb = $('.delButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , del);
	}
}

//�폜�N���b�N���̃C�x���g
function user_del(){

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//���[�UID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
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

//�폜�N���b�N���̃C�x���g
function del(){
/*
	var strMsg = '';
	strMsg += 'ID:['+ lclickSelElementsVar.children[1].innerHTML + ']';
	strMsg += '���f�[�^�x�[�X���폜���܂��B��낵���ł����B';
	//�m�F��ʏo��
	if(!window.confirm(strMsg)){
		return ;
	}
*/
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//���[�UID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','User_Id');
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

//�ǉ��{�^�������C�x���g
//��ʍ����I���s�̏�����ʉE���̃O���[�v���ɓ����
function add() {

	//���b�Z�[�W������
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");

	//�������Ȃ���Βǉ������Ȃ�
	//hidden��input����擾
	Authority_flg = document.getElementById("Authority_flg").value;
	if(Authority_flg != "1"){
		errJsCode = "ERRJ0017";
		//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
		$(this).blur() ;	//�{�^������t�H�[�J�X���O��
		//[$modal]���t�F�[�h�A�E�g������
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}

	//���̓`�F�b�N
	if(!input_check_insert('add')){
		//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
		$(this).blur() ;	//�{�^������t�H�[�J�X���O��
		//[$modal]���t�F�[�h�A�E�g������
//		$("#modal").fadeOut("slow");
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}

	//�m�F��ʏo��
	var User_Id = document.forms[0].User_Id;
	var strMsg = '';
	strMsg += '���[�UID:'+ User_Id.value +'���f�[�^�x�[�X�֓o�^���܂��B��낵���ł����B';
	if(!window.confirm(strMsg)){
		return ;
	}

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//���[�UID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Id.name);
	sendInp.setAttribute('value',User_Id.value);
	sendForm.appendChild( sendInp );

	//�p�X���[�h�擾
	var User_Password = document.forms[0].User_Password;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Password.name);
	sendInp.setAttribute('value',User_Password.value);
	sendForm.appendChild( sendInp );

	//�m�F�p�p�X���[�h�擾
	var User_Conf = document.forms[0].User_Conf;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Conf.name);
	sendInp.setAttribute('value',User_Conf.value);
	sendForm.appendChild( sendInp );

	//�����擾
	var Authority_Flg = document.forms[0].userAuthority;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','userAuthority');
	sendInp.setAttribute('value',Authority_Flg.value);
	sendForm.appendChild( sendInp );


	//�\�������擾
	var Disp_Num = document.forms[0].Disp_Num;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Disp_Num.name);
	sendInp.setAttribute('value',Disp_Num.value);
	sendForm.appendChild( sendInp );

	//�����t���O
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','add');
	sendForm.appendChild( sendInp );

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

//�X�V�{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă���O���[�v�ƁA����ɂɒǉ��E�폜���ꂽ���[�U���𑗂�
function update() {

	//���b�Z�[�W������
	outputMessage("","errMessageModal");
	outputMessage("","successMessage");

	//���̓`�F�b�N
	if(!input_check_insert('upd')){
		outputMessage(errJsCode,"errMessageModal");
		//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
		$(this).blur() ;	//�{�^������t�H�[�J�X���O��
		//[$modal]���t�F�[�h�A�E�g������
//		$("#modal").fadeOut("slow");
		return false;
	}

	//�m�F��ʏo��
	var User_Id = document.forms[0].User_Id;
	var strMsg = '';
	strMsg += '���[�UID:'+ User_Id.value +'���X�V���܂��B��낵���ł����B';
	if(!window.confirm(strMsg)){
		//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
		$(this).blur() ;	//�{�^������t�H�[�J�X���O��
		//[$modal]���t�F�[�h�A�E�g������
//		$("#modal").fadeOut("slow");
		return ;
	}

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.reset();
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	//�t�H�[���̃����N��ݒ�
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//���[�UID�擾
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Id.name);
	sendInp.setAttribute('value',User_Id.value);
	sendForm.appendChild( sendInp );

	//�p�X���[�h�擾
	var User_Password = document.forms[0].User_Password;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Password.name);
	sendInp.setAttribute('value',User_Password.value);
	sendForm.appendChild( sendInp );

	//�p�X���[�h�m�F�p�擾
	var User_Conf = document.forms[0].User_Conf;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',User_Conf.name);
	sendInp.setAttribute('value',User_Conf.value);
	sendForm.appendChild( sendInp );

	//�����擾
	var Authority_Flg = document.forms[0].userAuthority;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','Authority_Flg');
	sendInp.setAttribute('value',Authority_Flg.value);

	sendForm.appendChild( sendInp );


	//�\�������擾
	var Disp_Num = document.forms[0].Disp_Num;
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Disp_Num.name);
	sendInp.setAttribute('value',Disp_Num.value);
	sendForm.appendChild( sendInp );

	//�����t���O
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name','action');
	sendInp.setAttribute('value','mod');
	sendForm.appendChild( sendInp );

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;

}

//�߂�{�^���N���b�N�������̃C�x���g�𒣂�t����
function backButton(){
	var nb = $('.backButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , back);
	}
}

//�߂�{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă���O���[�v�ƁA����ɂɒǉ��E�폜���ꂽ���[�U���𑗂�
function back() {
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','get');
	sendForm.setAttribute('action','./DataView_user_conf.php');

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}

addEvent("load",window,delButton);
addEvent("load",window,backButton);



//���̓`�F�b�N
function input_check_insert(modeStr) {

	//�G���[���b�Z�[�W������
	errJsCode = "";
	outputMessage(errJsCode,"errMessageModal");
	outputMessage("","successMessage");

	//�l�擾
	User_Id = document.forms[0].User_Id.value;
	User_Password = document.forms[0].User_Password.value;
	User_Conf = document.forms[0].User_Conf.value;
	Disp_Num = document.forms[0].Disp_Num.value;

	//���̓`�F�b�N�󂩂ǂ���
	if(!input_check_Space(User_Id)){
		errJsCode = "ERRJ0001";
		return false;
	}
	//���̓`�F�b�N�p�������ǂ����i�L���ȂǕs�j
	if(!input_check_Alphanumeric(User_Id)){
		errJsCode = "ERRJ0002";
		return false;
	}
	//���͌������킩�ǂ���
	if(!input_check_length(User_Id,4,16)){
		errJsCode = "ERRJ0003";
		return false;
	}

//upd koyama password accept null
	if(modeStr == 'add' || User_Password != ''){
		//���̓`�F�b�N�󂩂ǂ���
		if(!input_check_Space(User_Password)){
			errJsCode = "ERRJ0004";
			return false;
		}
		//���̓`�F�b�N�p�������ǂ����i�L���ȂǕs�j
		if(!input_check_Alphanumeric(User_Password)){
			errJsCode = "ERRJ0005";
			return false;
		}
		//���͌������킩�ǂ���
		if(!input_check_length(User_Password,4,16)){
			errJsCode = "ERRJ0006";
			return false;
		}
	}
	//���̓`�F�b�N�󂩂ǂ���
	if(!input_check_Space(Disp_Num)){
		errJsCode = "ERRJ0007";
		return false;
	}
	//���̓`�F�b�N�������ǂ����i���̂݁j
	if(!input_check_Numeric(Disp_Num)){
		errJsCode = "ERRJ0008";
		return false;
	}
	//���͌������킩�ǂ���
	if(!input_check_length(Disp_Num,1,2)){
		errJsCode = "ERRJ0009";
		return false;
	}
	//���͐��l���͈͓����ǂ���
	if(!input_check_range(Disp_Num,15,99)){
		errJsCode = "ERRJ0010";
		return false;
	}

	if(modeStr == 'add'){
		//���[�UID�����ɑ��݂��Ă��邩�i��ʏ�̃��X�g�Əƍ��j
		var users = document.getElementById("userListBody");
		if(users){
			for(var i=0;i < users.children.length;i++){
				var targetIdTemp = users.children[i].children[1].innerHTML;
				if(User_Id == targetIdTemp || User_Id == "admin"){
					//���݂��Ă���ꍇ
					errJsCode = "ERRJ0011";
					return false;
				}
			}
		}
	}

	//�p�X���[�h�Ɗm�F�p�p�X���[�h����v���Ă��邩
	if(User_Password != User_Conf ){
		errJsCode = "ERRJ0012";
		return false;
	}
	return true;
}
