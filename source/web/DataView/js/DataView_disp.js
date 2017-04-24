var newCount  = 1;
var sendFlg   = false;
var csvExeFlg = false;
var interval_id = null;
var errCount = 0;

//�o�^�{�^���������̃C�x���g�𒣂�t����
function signButton(evt){
	var lb = $('.signButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , sign);
	}
}
addEvent('load',window,signButton);

//�����^�O�o�^���N�G�X�g
function sign() {
	var Command_Name = $('#Command_Name');
	var Sql_str = $('#Sql_str');
	var http1 = createXmlHttpRequest();
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	//���̓`�F�b�N
	//�󔒃`�F�b�N
	if(!input_check_Space(trim(Command_Name[0].value))){
		errJsCode = "ERRJ0052";
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
	//�֎~�����`�F�b�N
	if(!input_check_symbol(trim(Command_Name[0].value))){
		errJsCode = "ERRJ0053";
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
		
	
	if(http1) {
		http1.open("POST", "DataView_command_insert.php", true);
		
		http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		http1.onreadystatechange = function() {
			/*  �T�[�o�[���牞�������������̏���  */
			if(http1.readyState == 4 && http1.status == 200) {
				writeRes(http1.responseText);
				//���[�_���N���[�Y
				modalClose();
			}else{
				//�G���[���̏���
				//errJsCode = "ERRJ0056";
				//outputMessage(errJsCode,"errMessage");
			}
		}
		http1.send(
			"Command_Name=" + encodeURI(trim(Command_Name[0].value)) +
			"&Sql_str=" + encodeURI(Sql_str[0].value)
		);
	}
}

//�o�^���ʕ\��
function writeRes (ReturnValue) {
	//�G���[�R�[�h���A���Ă����Ƃ��̏�����ǉ����邱��
	messageInput = trim(ReturnValue);
	if( /^ERR(.)*/.test(messageInput)){
		outputMessage(messageInput,"errMessage");
	}else{
		outputMessage(messageInput,"successMessage");
	}
}

//�y�[�W�����N�������̃C�x���g�𒣂�t����
var linkButton = function () {
	var lb = $('.link');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , link);
	}
}

//�y�[�W�����N�������̃C�x���g
function link(evt){
	var pageNum;
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	if (!sendFlg && !csvExeFlg){
		pageNum = this.className.match( /page=([0-9]+) / );
		if((pageNum.length > 0)){
			
			//�t�H�[���쐬
			var sendForm = document.createElement('form');
			sendForm.className = "hidden";
			sendForm.setAttribute('method','post');
			sendForm.setAttribute('action','./DataView_disp.php?'+ pageNum[0]);
			
			//��ʂ̖߂��擾
			var pgName = document.getElementById('prePgName');
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',pgName.name);
			sendInp.setAttribute('value',pgName.value);
			sendForm.appendChild( sendInp );
			
			//�����^�OID
			var selectedCommandId = document.getElementById('selectedCommandId');
			if(selectedCommandId){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedCommandId.name);
				sendInp.setAttribute('value',selectedCommandId.value);
				sendForm.appendChild( sendInp );
			}
			
			//�I���e�[�u��
			var selectedTables = document.getElementsByName('selectedTables[]');
			for(var i=0;i < selectedTables.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedTables[i].name);
				sendInp.setAttribute('value',selectedTables[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//��������
			var selectedBonds = document.getElementsByName('selectedBonds[]');
			for(var i=0;i < selectedBonds.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedBonds[i].name);
				sendInp.setAttribute('value',selectedBonds[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//�\������
			var selectedItemNames = document.getElementsByName('selectedItemNames[]');
			for(var i=0;i < selectedItemNames.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedItemNames[i].name);
				sendInp.setAttribute('value',selectedItemNames[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//���o����
			var selectedFilters = document.getElementsByName('selectedFilters[]');
			for(var i=0;i < selectedFilters.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedFilters[i].name);
				sendInp.setAttribute('value',selectedFilters[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//���בւ�
			var selectedSorts = document.getElementsByName('selectedSorts[]');
			for(var i=0;i < selectedSorts.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedSorts[i].name);
				sendInp.setAttribute('value',selectedSorts[i].value);
				sendForm.appendChild( sendInp );
			}
			
			sendFlg = true;
			document.body.appendChild( sendForm );
			sendForm.submit();
			return false;
		}
	}else{
		if(sendFlg){
			errJsCode = "ERRJ0055";
			outputMessage(errJsCode,"errMessage");
			return false;
		}else{
			errJsCode = "ERRJ0054";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}
}
addEvent('load',window,linkButton);


//csv�쐬�{�^���������̃C�x���g�𒣂�t����
function csvButton(evt){
	var lb = $('.csvButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , csv);
	}
}
addEvent('load',window,csvButton);

//ajax�𗘗p����SCV�쐬���������s������
function csv(){
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	errCount = 0;
	
	//���s
	if (!csvExeFlg && !sendFlg){
	
		var Sql_str = $('#Sql_str');
		var Output_file = $('#Output_file');
		var http1 = createXmlHttpRequest();
		if(http1) {
			csvExeFlg = true;
			http1.open("POST", "DataView_create_csv_exe.php", true);
			http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
			http1.onreadystatechange = function() {
				/*  �T�[�o�[���牞�������������̏���  */
				if(http1.readyState == 4 && http1.status == 200) {
					//�������̏���
					res = trim(http1.responseText);
					if( /^ERR(.)*/.test(res)){
						outputMessage(res,"errMessage");
					}else{
						str = http1.responseText.split(",");
						//CSV�쐬���Ă���v���Z�XID���擾
						getPID = str[0].replace(/PID:/,"");
						//��ʁihidden�j�v���Z�XID����
						var PID = $('#PID');
						PID[0].value = getPID;
						//���[�_����ʕ\��
						$('#DLmodalWritingCount')[0].innerHTML = '0' ;
						DLmodalOpen();
						
						//����I�ɊĎ����������s������
						interval_id = setInterval('pid_check();',5000);
					}
				}else{
					//�G���[���̏���
					//csvExeFlg = false;
					//errJsCode = "ERRJ0056";
					//outputMessage(errJsCode,"errMessage");
				}
			}
			http1.send(
				"Sql_str=" + encodeURI(Sql_str[0].value) +
				"&Output_file=" + encodeURI(Output_file[0].value) 
			);
		}
	}else{
		if(sendFlg){
			errJsCode = "ERRJ0055";
			outputMessage(errJsCode,"errMessage");
			return false;
		}else{
			errJsCode = "ERRJ0054";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}
}



//pid�����s�����ǂ������ׂ�
function pid_check(){

	var PID             = $('#PID');
	var Output_file     = $('#Output_file');
	var getPIDinfo      = '';
	var getWritingCount = '';

	var http2 = createXmlHttpRequest();
	if(http2) {
		http2.open("POST", "DataView_pid_check.php", true);
		http2.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		http2.onreadystatechange = function() {
			/*  �T�[�o�[���牞�������������̏���  */
			if(http2.readyState == 4 && http2.status == 200) {
				//�������̏���
				http2.responseText;
				res = trim(http2.responseText);
				if( /^ERR(.)*/.test(res)){
					errCount = errCount + 1;
					if(errCount > 100){
						errJsCode = "ERRJ0057";
						outputMessage("errJsCode","errMessage");
						//CSV�쐬�����I��
						csvExeFlg = false;
						//���[�_���\��������
						setTimeout('DLmodalClose()',3000)
						// �^�C�}�[���~����
						clearInterval(interval_id);
					}
				}else{
					str = http2.responseText.split(",");
					//�v���Z�XID�̎��s��Ԏ擾
					getPIDinfo = str[0].replace(/PIDinfo:/,"");
					getWritingCount = str[1].replace(/WritingCount:/,"");
					//��ʂɃv���Z�XID�Ǝ擾�\�茏������
					$('#DLmodalWritingCount')[0].innerHTML = getWritingCount ;
					
					//���s�����m�F
					if(getPIDinfo != "true"){
						//���s���Ă��Ȃ��ꍇ
						//CSV�쐬�����I��
						csvExeFlg = false;
						//���[�_���\��������
						setTimeout('DLmodalClose()',3000)
						// �^�C�}�[���~����
						clearInterval(interval_id);
						//�_�E�����[�h�������J�n����B
						download_csv();
					}
				}
			}
		}
		http2.send(
			"PID=" + encodeURI(PID[0].value) +
			"&Output_file=" + encodeURI(Output_file[0].value)
		);
	}
}



//�_�E�����[�h���[�_��OPEN
function DLmodalOpen(){
	//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
	$(this).blur() ;  //�{�^������t�H�[�J�X���O��
	
	//���[�_�����t�F�[�h�C��������
	$("#DLmodal").fadeIn("slow");
	
	//���[�_����ʂ��Z���^�����O
	centeringModalSyncer("#DLmodalBody");
}


//�_�E�����[�h���[�_������
function DLmodalClose(){
	//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
	$(this).blur() ;	//�{�^������t�H�[�J�X���O��

	//[$modal]���t�F�[�h�C��������
	$("#DLmodal").fadeOut("slow");
}



//SCV���_�E�����[�h����
function download_csv(){
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_download.php');
	//�o�͐�t�@�C�����擾
	var Output_file = document.getElementById('Output_file');
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Output_file.name);
	sendInp.setAttribute('value',Output_file.value);
	sendForm.appendChild( sendInp );
	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
}
