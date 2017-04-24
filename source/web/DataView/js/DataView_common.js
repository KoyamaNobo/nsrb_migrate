var lclickSelElementsVar;
var rclickSelElementsVar;
var bclickSelElementsVar;


//�C�x���g�ϐ�����G�������g�����o��
//********************* input ***********:
//evt:�C�x���g�ϐ�
//********************* return **********:
//element:�C�x���g�����������G�������g
//********************* create **********:
//date    :20140111
//Author  :n.koyama
function evtToElement(evt){
	var element
	try{
		element  = window.event.srcElement;
	}catch( e ){
		element  = evt.currentTarget;
	}
	return element;
}


//����̃G�������g�ɃC�x���g��ǉ�
//********************* input ***********:
//evt  : �C�x���g��
//elem : �Ώۂ̃G�������g
//fn   : �t�@���N�V����(�N���[�W�������u��)
//********************* return **********:
function addEvent(evt , elem , fn){
	try {
		elem.addEventListener( evt, fn, false);
	} catch (e) {
		elem.attachEvent('on' + evt, fn);
	}
}

//����̃G�������g�ɃC�x���g��ǉ�
//********************* input ***********:
//evt  : �C�x���g��
//elem : �Ώۂ̃G�������g
//fn   : �t�@���N�V����(�N���[�W�������u��)
//********************* return **********:
function remEvent(evt , elem , fn){
	try {
	    elem.removeEventListener(evt, fn, false);
	} catch (e) {
	    eventTarget.detachEvent('on'+evt , fn);
	}
}

//����̃G�������g�擾
//********************* input ***********:
//elem      : �^�[�Q�b�g�Ƃ��Ă�root�G�������g
//classname : �ΏۂƂ���N���X��
//********************* return **********:
//targElem  :�擾���ꂽ�G�������g��array
//********************* create **********:
function getElementsByClassName( elem , classname){

  var targElem = new Array();

  if(elem){
    var tags = elem.getElementsByTagName('*');
  }else{
    var tags = document.getElementsByTagName('*');
  }

  for(var i = 0;i < tags.length;i++){
    var tmpElem = tags[i];
	var attrString = tmpElem.className;

    if(attrString){
      var attrArray = attrString.split(' ');
      if(attrArray){
        for(var j = 0;j < attrArray.length; j++){
          if(attrArray[j] == classname){
            targElem.push(tags[i]);
            break;
          }
        }
      }
    }
  }
  return targElem;
}

// �u���E�U��ʔ���p�Œ�l
var browserVer = {
	MSIE : { keys : [ 'MSIE;' ], value : 'MSIE;'},
	Firefox : { keys : [ 'Firefox' ], value : 'Firefox/'},
	Chrome : { keys : [ 'Chrome' ], value : 'Chrome/'},
	Opera : { keys : [ 'Opera/' ] , value : 'Opera/'},
	Safari : { keys: [ 'Safari',
						'Version'] ,value : 'Version/'}
};

/* �萔 */
LF  = String.fromCharCode(10);
COMMA = String.fromCharCode(44);

/*
 * �q�v�f���폜(����)
 *  ����:�폜�Ώۂ����e�m�[�h
 */
function RemoveChildItem (node) {
	c = node.firstChild;
	while (c) {
		node.removeChild(node.firstChild);
		c = node.firstChild;
	}
}

//�G�������g�������̃N���X���폜����֐�
function removeClass( elem, classname){

	var verChk  = getBrowserVer( browserVer.MSIE );

	if(elem == null){
		return;
	}

	if( verChk == "8.0" ){
		attrString = elem.getAttribute('class',2);
	}else{
		attrString = elem.className;
	}

	if(attrString){
		var attrArray = attrString.split(' ');
		if(attrArray){
			for(var j = 0;j < attrArray.length; j++){
				if(attrArray[j] == classname){
					attrArray.splice(j,1);
				}
			}

			var tmpClasses = '';
			for( i= 0 ;i < attrArray.length; i++  ){
				tmpClasses += attrArray[i] + ' ';
			}

			tmpClasses = trim( tmpClasses );

			if( verChk == "8.0" ){
				elem.setAttribute('class', tmpClasses);
			}else{
				elem.className = tmpClasses;
			}
		}
	}
}

// �A�N�Z�X����Ă���u���E�U�̃o�[�W������Ԃ��B
// (MSIE,Firefox,Chrome,Opera,Safari �̔���)
// �u���E�U��ʂ̎w�肪����ꍇ�́A��v�����ꍇ�̂݃o�[�W������Ԃ��B
function getBrowserVer( Key ){

	var distinctionBrowser = function( inagent , inkey ){
		var ret = true;
		for(var i = 0 ; i< inkey.length; i++){
			if(  inagent.indexOf( inkey[i] ) < 0 ){
				ret = false;
			}
		}
		return  ret;
	}

	var UserAgent = navigator.userAgent;
	var myAgent = UserAgent + ";";
	var ret = '';
	var isBrowser;

	if( Key == null){
		for( var keyString in browserVer ){
			if ( distinctionBrowser( myAgent ,browserVer[keyString]['keys'] ) ){
				myStart = myAgent.indexOf( browserVer[keyString]['value'] ) + browserVer[keyString]['value'].length;
				myEnd = myAgent.indexOf( ";", myStart );
				ret = myAgent.substring ( myStart, myEnd );
				break;
			}
		}
	}else{
		if ( distinctionBrowser( myAgent ,Key['keys'] ) ){
		  myStart = myAgent.indexOf( Key['value'] ) + Key['value'].length;
		  myEnd = myAgent.indexOf( ";", myStart );
		  ret = myAgent.substring ( myStart, myEnd );
		}
	}
	return ret;
}


// ������̃g����
function trim( str , param){

	if( param ){
		return str.replace(/(^[\s�@]+)|([\s�@]+$)/g, "");
	}else{
		return str.replace(/(^\s+)|(\s+$)/g, "");
	}

}

/**
 *  Ajax�֐� HTTP�N���C�A���g����(����)
 */
function createXmlHttpRequest() {
	var xmlhttp = null;
	try {
		xmlhttp = new XMLHttpRequest();
	} catch(e) {
		try {
			xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
		} catch(e) {
			try {
				xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
			} catch(e) {
				return null;
			}
		}
	}
	return xmlhttp;
}

//* ������ɁAHTML�G���R�[�h���s��
function htmlEntities(str) {
    return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

//���j���[�����N�������̃C�x���g�𒣂�t����"
function menuButton() {
	var ml = $('.menuLink');
	for(var i = 0; i < ml.length ;i++){
		addEvent("click", ml[i] , menuLink);
	}
}
addEvent("load", window , menuButton);

//�t�H�[�����쐬����GET���M����
function menuLink(evt){

	var menuURL;
	menuURL = this.getAttribute('name');
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','get');
	sendForm.setAttribute('action','./' + menuURL + '.php');

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
}
addEvent("load", window , menuButton);


//�\���{�^���������̃C�x���g�𒣂�t����
function summaryDispButton(evt){
	var lb = $('.summaryDisp');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , summaryDisp);
	}
}
addEvent('load',window,summaryDispButton);

//�N���X���̂Q�Ԗڂ��擾����B
//�擾�����N���X��\��������B
function summaryDisp(evt){
	var className = this.className.split(" ")[1];
	var dispTarget = ".description" + className;
	var strTarget  = "." + className;
	if ($(dispTarget).css('display') == 'block') {
		// �\������Ă���ꍇ�̏���
		$(dispTarget).hide();
		this.value="�ڍו\��";

	} else {
		// ��\���̏ꍇ�̏���
		$(dispTarget).show();
		this.value="��\��";
	}
}

//���X�g����I������Ă���s�̏����擾�E�ۑ�����֐�
var setClickElems = function( evt ) {
	var targetNode;
	var targetParent;

	try{
		targetNode = this;
		targetParent = this.parentNode;
		for(var i=0;i < targetParent.children.length;i++){
			var temp = targetParent.children[i].className;
			targetParent.children[i].className = targetParent.children[i].className.replace(/(\s)?select/g, "");
			var temp2 = targetParent.children[i].className;
		}
		targetNode.className = targetNode.className + ' select';
	}catch(e){

		if( existClassName( evt.srcElement , 'listElem') ){
			var targetNode = evt.srcElement;
		}else{
			var targetNode = findParentByClassName( evt.srcElement ,'listElem');
		}

		if (targetNode.parent.className.match(/leftList/i)){
			targetParent = findParentByClassName( targetNode ,'leftList');
		}else if(targetNode.parent.className.match(/rightList/i)){
			targetParent = findParentByClassName( targetNode ,'rightList');
		}else if(targetNode.parent.className.match(/sortList/i)){
			targetParent = findParentByClassName( targetNode ,'sortList');
		}else if(targetNode.parent.className.match(/commandList/i)){
			targetParent = findParentByClassName( targetNode ,'commandList');
		}else if(targetNode.parent.className.match(/filterList/i)){
			targetParent = findParentByClassName( targetNode ,'filterList');
		}

		for(var i=0;i < targetParent.children.length;i++){
			targetParent.children[i].className = targetParent.children[i].className.replace(/(\s)?select/g, "");
		}
		targetNode.className = targetNode.className + ' select';
	}

	if (targetParent.className.match(/leftList/i)){
		lclickSelElementsVar = targetNode;
	}else if(targetParent.className.match(/commandList/i)){
		lclickSelElementsVar = targetNode;
	}else if(targetParent.className.match(/sortList/i)){
		rclickSelElementsVar = targetNode;
	}else if(targetParent.className.match(/rightList/i)){
		rclickSelElementsVar = targetNode;
	}else if(targetParent.className.match(/filterList/i)){
		rclickSelElementsVar = targetNode;
	}else{
		bclickSelElementsVar = targetNode;
	}

};


//���X�g�A�C�e���N���X��
//�Z���N�g��Ԃ̃C�x���g�𒣂�t����
function selectElem() {
	var targetElements = $('.listElem');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("click", targetElements[i] , setClickElems);
	}
}
addEvent("load", window , selectElem);

//list�̓��o��
//author:koyama
//20150918
var keyDownCueList = function (event){
	var srcElem;
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	for(var ii = 0; ii < srcElem.children.length ;ii++){
		//�ŏ���text��innerHTML������q�̒��g����ɂ���
		var jj=0;
		var chldElem = srcElem.children[ii];
		for(jj=0;jj < chldElem.children.length;jj++){
			//��̎q�͌��Ȃ�
			if(chldElem.children[jj].innerHTML.replace(/(^\s+)|(\s+$)/g, "").length > 0){
				break;
			}
		}
		//jj�𒆐g������̂Ŏ~�߂ď���
		var targetText = chldElem.children[jj].innerHTML.replace(/(^\s+)|(\s+$)/g, "");
		if(targetText.substr(0,1).toLowerCase() == String.fromCharCode(event.keyCode).toLowerCase()){
			srcElem.scrollTop = ii * chldElem.scrollHeight;
			break;
		}
	}
}

//list�̓��o�������邽�߂ɃC�x���g��ǉ�
//author:koyama
//20150918
var listCue = function(){
	//���o���̑Ώۂ̑I��
	var targetElements = $('.leftList');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("keydown", targetElements[i]
			, function(event){
				return keyDownCueList(event);
			} );
	}
}


//�폜moda���popen function
//author:koyama
//date:20160607
function modal_delOpen(evt){
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//�ǂ̉�ʂ���Ă΂ꂽ������
	var pgName = location.pathname.split('/')[location.pathname.split('/').length - 1];
	if(pgName.match(/user_conf.php/i)){
		//���[�U�ݒ��ʂ���Ă΂ꂽ�ꍇ
		//�����I������Ă��Ȃ���΃��b�Z�[�W��\��
		if(lclickSelElementsVar == undefined){
			errJsCode = "ERRJ0014";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//�������Ȃ���΍폜�����Ȃ�
		//hidden��input����擾
		Authority_flg = document.getElementById("Authority_flg").value;
		if(Authority_flg != "1"){
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
		var msg = '';
		msg += '<p>ID:['+ lclickSelElementsVar.children[1].innerHTML + ']';
		msg += '���f�[�^�x�[�X���폜���܂��B</p><p>��낵���ł���?</p>'
		$('#modal_del span.del_message').html(msg)
	}
	//���[�_�����t�F�[�h�C��������
	$("#modal_del").fadeIn("slow");

	//���[�_����ʂ��Z���^�����O
	centeringModalSyncer("#modal_delBody");
}
//�폜moda���p���[�_��OPEN�{�^���������̃C�x���g�𒣂�t����
//author:koyama
//date:20160607
function modal_delOpenButton(evt){
	var lb = $('.modal_delOpenButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modal_delOpen);
	}
}

//�폜moda���p���[�_������
//author:koyama
//date:20160607
function modal_delClose(evt){
	//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
	$(this).blur() ;	//�{�^������t�H�[�J�X���O��
	//if($("#modal")[0]) return false ;		//�V�������[�_���E�B���h�E���N�����Ȃ� [���Ƃǂ��炩�I��]

	//[$modal]���t�F�[�h�A�E�g������
	$("#modal_del").fadeOut("slow");
}

//�폜moda���p���[�_��close�{�^���������̃C�x���g�𒣂�t����
//author:koyama
//date:20160607
function modal_delCloseButton(evt){
	var lb = $('#modal_del .modalCloseButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modal_delClose);
	}
}

//���[�_��OPEN�{�^���������̃C�x���g�𒣂�t����
function modalOpenButton(evt){
	var lb = $('.modalOpenButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modalOpen);
	}
}
//���[�_��OPEN
function modalOpen(evt){
	//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
	$(this).blur() ;	//�{�^������t�H�[�J�X���O��

	//���b�Z�[�W������
	errJsCode = "";
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");

	//��ʖ��擾
	var pgName = location.pathname.split('/')[location.pathname.split('/').length - 1];
	if(pgName.match(/item_conf.php/i)){
		//�a���ݒ�
		//���ڐݒ��ʂ̂Ƃ��̂�
		//�������Ȃ���Βǉ������Ȃ�
		Authority_flg = document.getElementById("Authority_flg").value;
		if(Authority_flg != "1"){
			errJsCode = "ERRJ0023";
			outputMessage(errJsCode,"errMessage");
			return false;
		}

		//�����I������Ă��Ȃ���΃��b�Z�[�W��\��
		if(lclickSelElementsVar == undefined){
				//�G���[���b�Z�[�W
				errJsCode = "ERRJ0022";
				outputMessage(errJsCode,"errMessage");
				return false;
		}

		//�I�����ڂ̘a�������[�_���̓��͗��ɕ\��
		var Japanese_Name = $('#Japanese_Name');
		if(lclickSelElementsVar.children[3].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			Japanese_Name[0].value = '';
		}else{
			Japanese_Name[0].value = lclickSelElementsVar.children[3].innerHTML;
		}
		//�I�����ڂ̔�\���ݒ�����[�_���̔�\���ݒ�ɕ\��
		if(lclickSelElementsVar.children[4].innerHTML.replace( /\s|&nbsp;/g , '') != ""){
			$('#NonDisp_Flg_no')[0].checked =false;
			$('#NonDisp_Flg_yes')[0].checked = true;
		}else{
			$('#NonDisp_Flg_yes')[0].checked = false;
			$('#NonDisp_Flg_no')[0].checked = true;
		}
	}
	if(pgName.match(/users_item.php/i)){
		//���ڐݒ�
		var updateSwitch=false;
		//event�𔍂����̂ɗL���֐��̂ق����s������������
		if(evtToElement(evt).className.match(/update/i)){
			updateSwitch=true;
			$('#modalButton .modeButton')[0].innerHTML = '�X�V';
			remEvent("click", $('#modalButton .modeButton')[0] , add);
			addEvent("click", $('#modalButton .modeButton')[0] , update);
		}else{
			$('#modalButton .modeButton')[0].innerHTML = '�ǉ�';
			remEvent("click", $('#modalButton .modeButton')[0] , update);
			addEvent("click", $('#modalButton .modeButton')[0] , add);
		}
		outputMessage("","errMessage");
		outputMessage("","successMessage");
		if(updateSwitch){
			//�����I������Ă��Ȃ���΃��b�Z�[�W��\������return
			if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0059";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
		//�������Ȃ���Ύ����̈ȊO�͕ҏW�����Ȃ�
		Authority_flg = document.getElementById("Authority_flg").value;
		if(updateSwitch == true && Authority_flg != "1" && (lclickSelElementsVar.children[8].innerHTML != $('#LoginId')[0].value) ){
			errJsCode = "ERRJ0023";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		if(!updateSwitch || lclickSelElementsVar.children[1].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Extension_Id')[0].value = '';
		}else{
			$('#Extension_Id')[0].value = lclickSelElementsVar.children[1].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[2].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Table_Name')[0].value = '';
		}else{
			$('#Table_Name')[0].value = lclickSelElementsVar.children[2].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[3].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Item_Japanese_Name')[0].value = '';
		}else{
			$('#Item_Japanese_Name')[0].value = lclickSelElementsVar.children[3].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[4].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#S_point')[0].value = '';
		}else{
			$('#S_point')[0].value = lclickSelElementsVar.children[4].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[5].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Size')[0].value = '';
		}else{
			$('#Size')[0].value = lclickSelElementsVar.children[5].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[6].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Data_Type')[0].value = '';
		}else{
			$('#Data_Type')[0].value = lclickSelElementsVar.children[6].innerHTML;
		}
		//�I�����ڂ̔�\���ݒ�����[�_���̔�\���ݒ�ɕ\��
		if(!updateSwitch || lclickSelElementsVar.children[7].innerHTML.match( /����/i)){
			$('#NonDisp_Flg_yes')[0].checked = false;
			$('#NonDisp_Flg_none')[0].checked = true;
		}else{
			$('#NonDisp_Flg_none')[0].checked =false;
			$('#NonDisp_Flg_yes')[0].checked = true;
		}
		if(!updateSwitch || lclickSelElementsVar.children[8].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#User_Id')[0].value = $('#LoginId')[0].value;
		}else{
			$('#User_Id')[0].value = lclickSelElementsVar.children[8].innerHTML;
		}
	}
	if(pgName.match(/user_conf.php/i)){
		//���[�U�ݒ�
		//���b�Z�[�W������
		//�ǉ����X�V���𔻕�
		var updateSwitch=false;
		if(evtToElement(evt).className.match(/update/i)){
			updateSwitch=true;
			$('#modalButton .modeButton')[0].innerHTML = '�X�V';
			remEvent("click", $('#modalButton .modeButton')[0] , add);
			addEvent("click", $('#modalButton .modeButton')[0] , update);
		}else{
			$('#modalButton .modeButton')[0].innerHTML = '�ǉ�';
			remEvent("click", $('#modalButton .modeButton')[0] , update);
			addEvent("click", $('#modalButton .modeButton')[0] , add);
		}
		outputMessage("","errMessage");
		outputMessage("","successMessage");
		if(updateSwitch){
			//�����I������Ă��Ȃ���΃��b�Z�[�W��\������return
			if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0013";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
			//�������Ȃ���Α��̃��[�U�͍X�V�����Ȃ�
			//hidden��input����擾
			Authority_flg = document.getElementById("Authority_flg").value;
			LoginId = document.getElementById("LoginId").value;
			if(LoginId != lclickSelElementsVar.children[1].innerHTML && Authority_flg != "1"){
				errJsCode = "ERRJ0016";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
// 			//�폜����Ă��鎞�̓��b�Z�[�W��\��
// 			if(lclickSelElementsVar.children[4].innerHTML == '�폜'){
// 				errJsCode = "ERRJ0018";
// 				outputMessage(errJsCode,"errMessage");
// 				return false;
// 			}
		}
		if(!updateSwitch || lclickSelElementsVar.children[1].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#User_Id')[0].disabled =false;
			$('#User_Id')[0].value = '';
		}else{
			$('#User_Id')[0].disabled =true;
			$('#User_Id')[0].value = lclickSelElementsVar.children[1].innerHTML;
		}
		if(!updateSwitch || lclickSelElementsVar.children[3].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#Disp_Num')[0].value = '';
		}else{
			$('#Disp_Num')[0].value = lclickSelElementsVar.children[3].innerHTML;
		}
		//�I�����ڂ̔�\���ݒ�����[�_���̔�\���ݒ�ɕ\��
		if(!updateSwitch || lclickSelElementsVar.children[2].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#userAuthority_yes')[0].checked = false;
			$('#userAuthority_none')[0].checked = true;
		}else{
			$('#userAuthority_none')[0].checked =false;
			$('#userAuthority_yes')[0].checked = true;
		}
		//�p�X���[�h�͎��Ȃ��̂ŁA�󔒂ɂ��Ă���
		$('#User_Password')[0].value = '';
		$('#User_Conf')[0].value = '';
	}

	//���[�_�����t�F�[�h�C��������
	$("#modal").fadeIn("slow");

	//���[�_����ʂ��Z���^�����O
	centeringModalSyncer("#modalBody");
}

//���[�_����ʂ��Z���^�����O������֐�
function centeringModalSyncer(target){

	//���(�E�B���h�E)�̕����擾���A�ϐ�[w]�Ɋi�[
	var w = $(window).width();

	//���(�E�B���h�E)�̍������擾���A�ϐ�[h]�Ɋi�[
	var h = $(window).height();

	//�R���e���c(target)�̕����擾���A�ϐ�[cw]�Ɋi�[
	var cw = $(target).outerWidth({margin:true});
	var cw = $(target).width();

	//�R���e���c(target)�̍������擾���A�ϐ�[ch]�Ɋi�[
	var ch = $(target).outerHeight({margin:true});
	var ch = $(target).height();

	//�R���e���c(target)��^�񒆂ɔz�u����̂ɁA���[���牽�s�N�Z�������΂������H���v�Z���āA�ϐ�[pxleft]�Ɋi�[
	var pxleft = ((w - cw)/2);

	//�R���e���c(target)��^�񒆂ɔz�u����̂ɁA�㕔���牽�s�N�Z�������΂������H���v�Z���āA�ϐ�[pxtop]�Ɋi�[
	var pxtop = ((h - ch)/2);

	//[target]��CSS��[left]�̒l(pxleft)��ݒ�
	$(target).css({"left": pxleft + "px"});

	//[target]��CSS��[top]�̒l(pxtop)��ݒ�
	$(target).css({"top": pxtop + "px"});

}

//���[�_��close�{�^���������̃C�x���g�𒣂�t����
function modalCloseButton(evt){
	var lb = $('#modal .modalCloseButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modalClose);
	}
}
//���[�_������
function modalClose(evt){
	//�L�[�{�[�h����Ȃǂɂ��A�I�[�o�[���C�����d�N������̂�h�~����
	$(this).blur() ;	//�{�^������t�H�[�J�X���O��
	//if($("#modal")[0]) return false ;		//�V�������[�_���E�B���h�E���N�����Ȃ� [���Ƃǂ��炩�I��]

	//[$modal]���t�F�[�h�A�E�g������
	$("#modal").fadeOut("slow");
}
//���[�_������̃C�x���g�\��t��
addEvent('load',window,modalCloseButton);
addEvent('load',window,modalOpenButton);



// ���ڎ擾��HTTP���N�G�X�g
function getItemValue(srcElem,tableName,idName,startNum) {
	srcElem.disabled = true;
	var http1 = createXmlHttpRequest();
	if(http1) {
		http1.open("POST", "searchItem.php", true);
		http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		http1.onreadystatechange = function() {
			/*  �T�[�o�[���牞�������������̏���  */
			if(http1.readyState == 4 ) {
				if(http1.status == 200){
					listWrite(http1.responseText,idName);
				}
				srcElem.disabled = false;
			}else{
				//�G���[���̏���
				//errJsCode = "ERRJ0056";
				//outputMessage(errJsCode,"errMessage");
			}
		}
		http1.send(
			"tableName=" + encodeURI(tableName) +
			"&startNum=" + encodeURI(startNum)  +
			"&srcUrl="   + encodeURI(location.pathname.split('/')[location.pathname.split('/').length - 1])
		);
	}
}

//���̓`�F�b�N�p�������ǂ����i�L���ȂǕs�j
function input_check_Alphanumeric(str) {
	if( /^[a-zA-Z0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//���̓`�F�b�N�p�������ǂ����i�L���Ȃǉ\�j
function input_check_Alphanumeric_ex(str) {
	if( /^[a-zA-Z0-9 !"#$%&'()=~|`{+*}<>?_\-^\\@\[;:\],./\\]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//���̓`�F�b�N�p�����ǂ����i�L���ȂǕt���j
function input_check_Alpha(str) {
	if( /^[a-zA-Z]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}

//���̓`�F�b�N�������ǂ����i���̂݁j
function input_check_Numeric(str) {
	if( /^[0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//���̓`�F�b�N�������ǂ����i���������j
function input_check_Numeric_Minus(str) {
	if( /^[-]?[0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}

//���̓`�F�b�N�󂩂ǂ���
function input_check_Space(str) {
	if( str ){
		return true;
	}else{
		return false;
	}
}

//���͌������킩�ǂ���
function input_check_length(str,min,max) {
	min = parseInt(min);
	max = parseInt(max);
	if( str.length >= min && str.length <= max ){
		return true;
	}else{
		return false;
	}
}
//���͐��l���͈͓����ǂ���
function input_check_range(str,min,max) {

	if( parseInt(str) >= parseInt(min) && parseInt(str) <= parseInt(max) ){
		return true;
	}else{
		return false;
	}
}

//���͂ɓ���L���i�֎~������j�������Ă��邩�ǂ���
function input_check_symbol(str) {
	if( /([<>&"'`~])/.test(str) ){
		return false;
	}else{
		return true;
	}
}

//���b�Z�[�W��C�Ӂiid�j�̏ꏊ�ɏo�͂���'
function outputMessage(code,ElementId) {
	Elem = document.getElementById(ElementId);
	if(Elem != undefined ){
		if(code){
			Elem.innerHTML = document.getElementById(code).value;
		}else{
			Elem.innerHTML = "";
		}
	}
}


function getItem() {
	//�ύX�����Z���N�g�v�f��ID���擾
	var idName = this.id;
	//�I�������e�[�u�����擾
	var tableName = this.options[this.selectedIndex].value;
	//�N���X�ɂ���ď���������ꏊ��ς���
	if (idName.match(/lTableName/i)){
		lclickSelElementsVar   = undefined;
		changeList    = document.getElementById('leftList');        //���X�g��ύX����v�f
		candidate_Add = document.getElementById('lCandidate_Add');  //���ǉ��{�^����z�u����v�f
		getStartNum   = document.getElementById('lStartNum');       //���ݕ\�����Ă��錏��������v�f
	}else if(idName.match(/rTableName/i)){
		rclickSelElementsVar = undefined;
		changeList    = document.getElementById('rightList');       //���X�g��ύX����v�f
		candidate_Add = document.getElementById('rCandidate_Add');  //���ǉ��{�^����z�u����̈�
		getStartNum   = document.getElementById('rStartNum');       //���ݕ\�����Ă��錏��������v�f
	}
	//���X�g�E���ǉ��{�^���E���ݕ\�����Ă��錏���̏�����
	RemoveChildItem(changeList);
	candidate_Add.innerHTML="";
	getStartNum.value= "0";

	if(tableName == ""){
		return false;
	}

	getItemValue(this,tableName,idName,0);
}

//���ǉ��{�^���������̃C�x���g
function getItemadd(event) {
	var lselectTableName;
	var srcElem;
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	//�N���b�N�����{�^����ID�ō��E����
	if(srcElem.id == "litemAdd"){
		//���̃{�^���̂Ƃ��i���̏�Ԏ擾�j
		lselectTableName = document.getElementById('lTableName');
		getStartNum = document.getElementById('lStartNum');
	}else if(srcElem.id == "ritemAdd"){
		//�E�̃{�^���̂Ƃ��i�E�̏�Ԏ擾�j
		lselectTableName = document.getElementById('rTableName');
		getStartNum = document.getElementById('rStartNum');
	}
	getItemValue(this,lselectTableName.value,lselectTableName.id,parseInt(getStartNum.value));
}


//�e�[�u���ύX���̃C�x���g�𒣂�t����
function tableNameOnchange() {
	var tn = $('.selectTableName');
	for(var i = 0; i < tn.length ;i++){
		addEvent("change", tn[i] , getItem);
	}
}

//���ڏ������X�g�ɕ\��
function listWrite (ReturnValue,idName) {
	var ValueData = ReturnValue.split(LF);
	var changeList;
	var button;
	var getStartNum;
	var ValueDataLength = 0; //��s���A���Ă���\��������̂ŁA�f�[�^���J�E���g 2015

	//�ύX���郊�X�g���擾
	if (idName.match(/lTableName/i)){
		changeList =  document.getElementById('leftList');//���X�g��ǉ�����ꏊ
		button =  document.getElementById('lCandidate_Add');//�ǉ��{�^����z�u����ꏊ
		getStartNum = document.getElementById('lStartNum');
	}else{
		changeList =  document.getElementById('rightList');//���X�g��ǉ�����ꏊ
		button =  document.getElementById('rCandidate_Add');//�ǉ��{�^����z�u����ꏊ
		getStartNum = document.getElementById('rStartNum');
	}

	//�G���[�R�[�h���A���Ă����Ƃ��̏�����ǉ����邱��
	if(ReturnValue.match(/^ERR/)){
		outputMessage(trim(ReturnValue),"errMessage");
		return false;
	}

	for (i=0; i<ValueData.length; i++) {
		if(ValueData[i] != "") {
			liItem = document.createElement('div');
			if((i % 2) == 0){
				liItem.className = "listElem";
			} else {
				liItem.className = "listElem odd";
			}
			OptionData = ValueData[i].split('//');

			s0 = document.createElement('span');
			s0.className = 'hidden bgChItems';
			s0.innerHTML = '';
			liItem.appendChild(s0);
			s1 = document.createElement('span');
			s1.className = 'hidden listTableName bgChItems';
			s1.innerHTML = OptionData[1];
			liItem.appendChild(s1);
			s2 = document.createElement('span');
			s2.className = 'listItemName bgChItems';
			s2.title     = '�e�[�u���� : ' + OptionData[1] + '\n�^ : ' + OptionData[5];
			s2.innerHTML = OptionData[2];
			liItem.appendChild(s2);

			s3 = document.createElement('span');
			s3.className = 'listJapaneseName bgChItems';
			s3.title     = '�e�[�u���� : ' + OptionData[1] + '\n�^ : ' + OptionData[5];
			s3.innerHTML = OptionData[6].replace(/[\n\r]/g,"");
			liItem.appendChild(s3);

			s4 = document.createElement('span');
			s4.className = 'hidden listId bgChItems';
			s4.innerHTML = OptionData[0];
			liItem.appendChild(s4);
			s5 = document.createElement('span');
			s5.className = 'listS_point right bgChItems';
			s5.title     = '�e�[�u���� : ' + OptionData[1] + '\n�^ : ' + OptionData[5];
			s5.innerHTML = OptionData[3];
			liItem.appendChild(s5);
			s6 = document.createElement('span');
			s6.className = 'listSize right bgChItems';
			s6.title     = '�e�[�u���� : ' + OptionData[1] + '\n�^ : ' + OptionData[5];
			s6.innerHTML = OptionData[4];
			liItem.appendChild(s6);

			s7 = document.createElement('span');
			s7.className = 'hidden listType bgChItems';
			s7.innerHTML = OptionData[5];
			liItem.appendChild(s7);

			changeList.appendChild(liItem);

			s8 = document.createElement('span');
			s8.className = 'hidden bgChItems id';
			s8.innerHTML = OptionData[0];
			liItem.appendChild(s8);
			changeList.appendChild(liItem);

			addEvent("click", liItem , setClickElems);
			ValueDataLength++;
		}
	}

	//����Ɍ��擾�{�^���ǉ�
	if (idName.match(/lTableName/i)){
		button.innerHTML= '<img src="./img/btntbn04-9.png" id="litemAdd" class="itemAdd" alt="���ǉ�" >';
		var itemAdd = document.getElementById("litemAdd");
	}else{
		button.innerHTML= '<img src="./img/btntbn04-9.png" id="ritemAdd" class="itemAdd" alt="���ǉ�" >';
		var itemAdd = document.getElementById("ritemAdd");
	}

	addEvent("click", itemAdd , function(event){ return getItemadd(event);});

	//DB���擾�����������X�V
	getStartNum.value = String(parseInt(getStartNum.value) + ValueDataLength);
}

addEvent('load',window,tableNameOnchange);

//F5�֎~
window.document.onkeydown = function (e)
{
  if (e != undefined)
  {
    if (e.keyCode == 116)
    {
      e.stopPropagation();
      e.preventDefault();
      e.keyCode = null;
      return false;
    }
  }
  else
  {
    if (event.keyCode == 116)
    {
      event.keyCode = null;
      return false;
    }
  }
}

//���[�_������̃C�x���g�\��t��
addEvent('load',window,modal_delOpenButton);
addEvent('load',window,modal_delCloseButton);
addEvent("load", window , listCue);
