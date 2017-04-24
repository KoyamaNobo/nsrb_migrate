var lclickSelElementsVar;
var rclickSelElementsVar;
var bclickSelElementsVar;


//イベント変数からエレメントを取り出す
//********************* input ***********:
//evt:イベント変数
//********************* return **********:
//element:イベントが発生したエレメント
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


//特定のエレメントにイベントを追加
//********************* input ***********:
//evt  : イベント名
//elem : 対象のエレメント
//fn   : ファンクション(クロージャか直置き)
//********************* return **********:
function addEvent(evt , elem , fn){
	try {
		elem.addEventListener( evt, fn, false);
	} catch (e) {
		elem.attachEvent('on' + evt, fn);
	}
}

//特定のエレメントにイベントを追加
//********************* input ***********:
//evt  : イベント名
//elem : 対象のエレメント
//fn   : ファンクション(クロージャか直置き)
//********************* return **********:
function remEvent(evt , elem , fn){
	try {
	    elem.removeEventListener(evt, fn, false);
	} catch (e) {
	    eventTarget.detachEvent('on'+evt , fn);
	}
}

//特定のエレメント取得
//********************* input ***********:
//elem      : ターゲットとしてのrootエレメント
//classname : 対象とするクラス名
//********************* return **********:
//targElem  :取得されたエレメントのarray
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

// ブラウザ種別判定用固定値
var browserVer = {
	MSIE : { keys : [ 'MSIE;' ], value : 'MSIE;'},
	Firefox : { keys : [ 'Firefox' ], value : 'Firefox/'},
	Chrome : { keys : [ 'Chrome' ], value : 'Chrome/'},
	Opera : { keys : [ 'Opera/' ] , value : 'Opera/'},
	Safari : { keys: [ 'Safari',
						'Version'] ,value : 'Version/'}
};

/* 定数 */
LF  = String.fromCharCode(10);
COMMA = String.fromCharCode(44);

/*
 * 子要素を削除(共通)
 *  入力:削除対象を持つ親ノード
 */
function RemoveChildItem (node) {
	c = node.firstChild;
	while (c) {
		node.removeChild(node.firstChild);
		c = node.firstChild;
	}
}

//エレメントから特定のクラスを削除する関数
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

// アクセスされているブラウザのバージョンを返す。
// (MSIE,Firefox,Chrome,Opera,Safari の判別)
// ブラウザ種別の指定がある場合は、一致した場合のみバージョンを返す。
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


// 文字列のトリム
function trim( str , param){

	if( param ){
		return str.replace(/(^[\s　]+)|([\s　]+$)/g, "");
	}else{
		return str.replace(/(^\s+)|(\s+$)/g, "");
	}

}

/**
 *  Ajax関数 HTTPクライアント生成(共通)
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

//* 文字列に、HTMLエンコードを行う
function htmlEntities(str) {
    return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

//メニューリンク押下時のイベントを張り付ける"
function menuButton() {
	var ml = $('.menuLink');
	for(var i = 0; i < ml.length ;i++){
		addEvent("click", ml[i] , menuLink);
	}
}
addEvent("load", window , menuButton);

//フォームを作成してGET送信する
function menuLink(evt){

	var menuURL;
	menuURL = this.getAttribute('name');
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','get');
	sendForm.setAttribute('action','./' + menuURL + '.php');

	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
}
addEvent("load", window , menuButton);


//表示ボタン押下時のイベントを張り付ける
function summaryDispButton(evt){
	var lb = $('.summaryDisp');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , summaryDisp);
	}
}
addEvent('load',window,summaryDispButton);

//クラス名の２番目を取得する。
//取得したクラスを表示させる。
function summaryDisp(evt){
	var className = this.className.split(" ")[1];
	var dispTarget = ".description" + className;
	var strTarget  = "." + className;
	if ($(dispTarget).css('display') == 'block') {
		// 表示されている場合の処理
		$(dispTarget).hide();
		this.value="詳細表示";

	} else {
		// 非表示の場合の処理
		$(dispTarget).show();
		this.value="非表示";
	}
}

//リストから選択されている行の情報を取得・保存する関数
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


//リストアイテムクラスに
//セレクト状態のイベントを張り付ける
function selectElem() {
	var targetElements = $('.listElem');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("click", targetElements[i] , setClickElems);
	}
}
addEvent("load", window , selectElem);

//listの頭出し
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
		//最初にtextのinnerHTMLがある子の中身を基準にする
		var jj=0;
		var chldElem = srcElem.children[ii];
		for(jj=0;jj < chldElem.children.length;jj++){
			//空の子は見ない
			if(chldElem.children[jj].innerHTML.replace(/(^\s+)|(\s+$)/g, "").length > 0){
				break;
			}
		}
		//jjを中身があるので止めて処理
		var targetText = chldElem.children[jj].innerHTML.replace(/(^\s+)|(\s+$)/g, "");
		if(targetText.substr(0,1).toLowerCase() == String.fromCharCode(event.keyCode).toLowerCase()){
			srcElem.scrollTop = ii * chldElem.scrollHeight;
			break;
		}
	}
}

//listの頭出しをするためにイベントを追加
//author:koyama
//20150918
var listCue = function(){
	//頭出しの対象の選択
	var targetElements = $('.leftList');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("keydown", targetElements[i]
			, function(event){
				return keyDownCueList(event);
			} );
	}
}


//削除modaｌ用open function
//author:koyama
//date:20160607
function modal_delOpen(evt){
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//どの画面から呼ばれたか判定
	var pgName = location.pathname.split('/')[location.pathname.split('/').length - 1];
	if(pgName.match(/user_conf.php/i)){
		//ユーザ設定画面から呼ばれた場合
		//何も選択されていなければメッセージを表示
		if(lclickSelElementsVar == undefined){
			errJsCode = "ERRJ0014";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//権限がなければ削除させない
		//hiddenのinputから取得
		Authority_flg = document.getElementById("Authority_flg").value;
		if(Authority_flg != "1"){
			errJsCode = "ERRJ0015";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//削除されている時はメッセージを表示
		if(lclickSelElementsVar.children[5].innerHTML == '削除'){
			errJsCode = "ERRJ0018";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		var msg = '';
		msg += '<p>ID:['+ lclickSelElementsVar.children[1].innerHTML + ']';
		msg += 'をデータベースより削除します。</p><p>よろしいですか?</p>'
		$('#modal_del span.del_message').html(msg)
	}
	//モーダルをフェードインさせる
	$("#modal_del").fadeIn("slow");

	//モーダル画面をセンタリング
	centeringModalSyncer("#modal_delBody");
}
//削除modaｌ用モーダルOPENボタン押下時のイベントを張り付ける
//author:koyama
//date:20160607
function modal_delOpenButton(evt){
	var lb = $('.modal_delOpenButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modal_delOpen);
	}
}

//削除modaｌ用モーダル閉じる
//author:koyama
//date:20160607
function modal_delClose(evt){
	//キーボード操作などにより、オーバーレイが多重起動するのを防止する
	$(this).blur() ;	//ボタンからフォーカスを外す
	//if($("#modal")[0]) return false ;		//新しくモーダルウィンドウを起動しない [下とどちらか選択]

	//[$modal]をフェードアウトさせる
	$("#modal_del").fadeOut("slow");
}

//削除modaｌ用モーダルcloseボタン押下時のイベントを張り付ける
//author:koyama
//date:20160607
function modal_delCloseButton(evt){
	var lb = $('#modal_del .modalCloseButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modal_delClose);
	}
}

//モーダルOPENボタン押下時のイベントを張り付ける
function modalOpenButton(evt){
	var lb = $('.modalOpenButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modalOpen);
	}
}
//モーダルOPEN
function modalOpen(evt){
	//キーボード操作などにより、オーバーレイが多重起動するのを防止する
	$(this).blur() ;	//ボタンからフォーカスを外す

	//メッセージ初期化
	errJsCode = "";
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");

	//画面名取得
	var pgName = location.pathname.split('/')[location.pathname.split('/').length - 1];
	if(pgName.match(/item_conf.php/i)){
		//和名設定
		//項目設定画面のときのみ
		//権限がなければ追加させない
		Authority_flg = document.getElementById("Authority_flg").value;
		if(Authority_flg != "1"){
			errJsCode = "ERRJ0023";
			outputMessage(errJsCode,"errMessage");
			return false;
		}

		//何も選択されていなければメッセージを表示
		if(lclickSelElementsVar == undefined){
				//エラーメッセージ
				errJsCode = "ERRJ0022";
				outputMessage(errJsCode,"errMessage");
				return false;
		}

		//選択項目の和名をモーダルの入力欄に表示
		var Japanese_Name = $('#Japanese_Name');
		if(lclickSelElementsVar.children[3].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			Japanese_Name[0].value = '';
		}else{
			Japanese_Name[0].value = lclickSelElementsVar.children[3].innerHTML;
		}
		//選択項目の非表示設定をモーダルの非表示設定に表示
		if(lclickSelElementsVar.children[4].innerHTML.replace( /\s|&nbsp;/g , '') != ""){
			$('#NonDisp_Flg_no')[0].checked =false;
			$('#NonDisp_Flg_yes')[0].checked = true;
		}else{
			$('#NonDisp_Flg_yes')[0].checked = false;
			$('#NonDisp_Flg_no')[0].checked = true;
		}
	}
	if(pgName.match(/users_item.php/i)){
		//項目設定
		var updateSwitch=false;
		//eventを剥がすのに有名関数のほうが都合がいいため
		if(evtToElement(evt).className.match(/update/i)){
			updateSwitch=true;
			$('#modalButton .modeButton')[0].innerHTML = '更新';
			remEvent("click", $('#modalButton .modeButton')[0] , add);
			addEvent("click", $('#modalButton .modeButton')[0] , update);
		}else{
			$('#modalButton .modeButton')[0].innerHTML = '追加';
			remEvent("click", $('#modalButton .modeButton')[0] , update);
			addEvent("click", $('#modalButton .modeButton')[0] , add);
		}
		outputMessage("","errMessage");
		outputMessage("","successMessage");
		if(updateSwitch){
			//何も選択されていなければメッセージを表示してreturn
			if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0059";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
		//権限がなければ自分の以外は編集させない
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
		//選択項目の非表示設定をモーダルの非表示設定に表示
		if(!updateSwitch || lclickSelElementsVar.children[7].innerHTML.match( /自分/i)){
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
		//ユーザ設定
		//メッセージ初期化
		//追加か更新かを判別
		var updateSwitch=false;
		if(evtToElement(evt).className.match(/update/i)){
			updateSwitch=true;
			$('#modalButton .modeButton')[0].innerHTML = '更新';
			remEvent("click", $('#modalButton .modeButton')[0] , add);
			addEvent("click", $('#modalButton .modeButton')[0] , update);
		}else{
			$('#modalButton .modeButton')[0].innerHTML = '追加';
			remEvent("click", $('#modalButton .modeButton')[0] , update);
			addEvent("click", $('#modalButton .modeButton')[0] , add);
		}
		outputMessage("","errMessage");
		outputMessage("","successMessage");
		if(updateSwitch){
			//何も選択されていなければメッセージを表示してreturn
			if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0013";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
			//権限がなければ他のユーザは更新させない
			//hiddenのinputから取得
			Authority_flg = document.getElementById("Authority_flg").value;
			LoginId = document.getElementById("LoginId").value;
			if(LoginId != lclickSelElementsVar.children[1].innerHTML && Authority_flg != "1"){
				errJsCode = "ERRJ0016";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
// 			//削除されている時はメッセージを表示
// 			if(lclickSelElementsVar.children[4].innerHTML == '削除'){
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
		//選択項目の非表示設定をモーダルの非表示設定に表示
		if(!updateSwitch || lclickSelElementsVar.children[2].innerHTML.replace( /\s|&nbsp;/g , '') == ""){
			$('#userAuthority_yes')[0].checked = false;
			$('#userAuthority_none')[0].checked = true;
		}else{
			$('#userAuthority_none')[0].checked =false;
			$('#userAuthority_yes')[0].checked = true;
		}
		//パスワードは取れないので、空白にしておく
		$('#User_Password')[0].value = '';
		$('#User_Conf')[0].value = '';
	}

	//モーダルをフェードインさせる
	$("#modal").fadeIn("slow");

	//モーダル画面をセンタリング
	centeringModalSyncer("#modalBody");
}

//モーダル画面をセンタリングをする関数
function centeringModalSyncer(target){

	//画面(ウィンドウ)の幅を取得し、変数[w]に格納
	var w = $(window).width();

	//画面(ウィンドウ)の高さを取得し、変数[h]に格納
	var h = $(window).height();

	//コンテンツ(target)の幅を取得し、変数[cw]に格納
	var cw = $(target).outerWidth({margin:true});
	var cw = $(target).width();

	//コンテンツ(target)の高さを取得し、変数[ch]に格納
	var ch = $(target).outerHeight({margin:true});
	var ch = $(target).height();

	//コンテンツ(target)を真ん中に配置するのに、左端から何ピクセル離せばいいか？を計算して、変数[pxleft]に格納
	var pxleft = ((w - cw)/2);

	//コンテンツ(target)を真ん中に配置するのに、上部から何ピクセル離せばいいか？を計算して、変数[pxtop]に格納
	var pxtop = ((h - ch)/2);

	//[target]のCSSに[left]の値(pxleft)を設定
	$(target).css({"left": pxleft + "px"});

	//[target]のCSSに[top]の値(pxtop)を設定
	$(target).css({"top": pxtop + "px"});

}

//モーダルcloseボタン押下時のイベントを張り付ける
function modalCloseButton(evt){
	var lb = $('#modal .modalCloseButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , modalClose);
	}
}
//モーダル閉じる
function modalClose(evt){
	//キーボード操作などにより、オーバーレイが多重起動するのを防止する
	$(this).blur() ;	//ボタンからフォーカスを外す
	//if($("#modal")[0]) return false ;		//新しくモーダルウィンドウを起動しない [下とどちらか選択]

	//[$modal]をフェードアウトさせる
	$("#modal").fadeOut("slow");
}
//モーダル制御のイベント貼り付け
addEvent('load',window,modalCloseButton);
addEvent('load',window,modalOpenButton);



// 項目取得のHTTPリクエスト
function getItemValue(srcElem,tableName,idName,startNum) {
	srcElem.disabled = true;
	var http1 = createXmlHttpRequest();
	if(http1) {
		http1.open("POST", "searchItem.php", true);
		http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		http1.onreadystatechange = function() {
			/*  サーバーから応答があった時の処理  */
			if(http1.readyState == 4 ) {
				if(http1.status == 200){
					listWrite(http1.responseText,idName);
				}
				srcElem.disabled = false;
			}else{
				//エラー時の処理
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

//入力チェック英数字かどうか（記号など不可）
function input_check_Alphanumeric(str) {
	if( /^[a-zA-Z0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//入力チェック英数字かどうか（記号など可能）
function input_check_Alphanumeric_ex(str) {
	if( /^[a-zA-Z0-9 !"#$%&'()=~|`{+*}<>?_\-^\\@\[;:\],./\\]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//入力チェック英数かどうか（記号など付加）
function input_check_Alpha(str) {
	if( /^[a-zA-Z]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}

//入力チェック数字かどうか（正のみ）
function input_check_Numeric(str) {
	if( /^[0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}
//入力チェック数字かどうか（負も許す）
function input_check_Numeric_Minus(str) {
	if( /^[-]?[0-9]+$/.test(str) ){
		return true;
	}else{
		return false;
	}
}

//入力チェック空かどうか
function input_check_Space(str) {
	if( str ){
		return true;
	}else{
		return false;
	}
}

//入力桁が正常かどうか
function input_check_length(str,min,max) {
	min = parseInt(min);
	max = parseInt(max);
	if( str.length >= min && str.length <= max ){
		return true;
	}else{
		return false;
	}
}
//入力数値が範囲内かどうか
function input_check_range(str,min,max) {

	if( parseInt(str) >= parseInt(min) && parseInt(str) <= parseInt(max) ){
		return true;
	}else{
		return false;
	}
}

//入力に特殊記号（禁止文字列）が入っているかどうか
function input_check_symbol(str) {
	if( /([<>&"'`~])/.test(str) ){
		return false;
	}else{
		return true;
	}
}

//メッセージを任意（id）の場所に出力する'
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
	//変更したセレクト要素のIDを取得
	var idName = this.id;
	//選択したテーブル名取得
	var tableName = this.options[this.selectedIndex].value;
	//クラスによって初期化する場所を変える
	if (idName.match(/lTableName/i)){
		lclickSelElementsVar   = undefined;
		changeList    = document.getElementById('leftList');        //リストを変更する要素
		candidate_Add = document.getElementById('lCandidate_Add');  //候補追加ボタンを配置する要素
		getStartNum   = document.getElementById('lStartNum');       //現在表示している件数を入れる要素
	}else if(idName.match(/rTableName/i)){
		rclickSelElementsVar = undefined;
		changeList    = document.getElementById('rightList');       //リストを変更する要素
		candidate_Add = document.getElementById('rCandidate_Add');  //候補追加ボタンを配置する領域
		getStartNum   = document.getElementById('rStartNum');       //現在表示している件数を入れる要素
	}
	//リスト・候補追加ボタン・現在表示している件数の初期化
	RemoveChildItem(changeList);
	candidate_Add.innerHTML="";
	getStartNum.value= "0";

	if(tableName == ""){
		return false;
	}

	getItemValue(this,tableName,idName,0);
}

//候補追加ボタン押下時のイベント
function getItemadd(event) {
	var lselectTableName;
	var srcElem;
	if(event.srcElement){
		srcElem = event.srcElement;
	}else {
		srcElem = event.currentTarget;
	}
	//クリックしたボタンのIDで左右判定
	if(srcElem.id == "litemAdd"){
		//左のボタンのとき（左の状態取得）
		lselectTableName = document.getElementById('lTableName');
		getStartNum = document.getElementById('lStartNum');
	}else if(srcElem.id == "ritemAdd"){
		//右のボタンのとき（右の状態取得）
		lselectTableName = document.getElementById('rTableName');
		getStartNum = document.getElementById('rStartNum');
	}
	getItemValue(this,lselectTableName.value,lselectTableName.id,parseInt(getStartNum.value));
}


//テーブル変更時のイベントを張り付ける
function tableNameOnchange() {
	var tn = $('.selectTableName');
	for(var i = 0; i < tn.length ;i++){
		addEvent("change", tn[i] , getItem);
	}
}

//項目情報をリストに表示
function listWrite (ReturnValue,idName) {
	var ValueData = ReturnValue.split(LF);
	var changeList;
	var button;
	var getStartNum;
	var ValueDataLength = 0; //空行が帰ってくる可能性があるので、データをカウント 2015

	//変更するリストを取得
	if (idName.match(/lTableName/i)){
		changeList =  document.getElementById('leftList');//リストを追加する場所
		button =  document.getElementById('lCandidate_Add');//追加ボタンを配置する場所
		getStartNum = document.getElementById('lStartNum');
	}else{
		changeList =  document.getElementById('rightList');//リストを追加する場所
		button =  document.getElementById('rCandidate_Add');//追加ボタンを配置する場所
		getStartNum = document.getElementById('rStartNum');
	}

	//エラーコードが帰ってきたときの処理を追加すること
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
			s2.title     = 'テーブル名 : ' + OptionData[1] + '\n型 : ' + OptionData[5];
			s2.innerHTML = OptionData[2];
			liItem.appendChild(s2);

			s3 = document.createElement('span');
			s3.className = 'listJapaneseName bgChItems';
			s3.title     = 'テーブル名 : ' + OptionData[1] + '\n型 : ' + OptionData[5];
			s3.innerHTML = OptionData[6].replace(/[\n\r]/g,"");
			liItem.appendChild(s3);

			s4 = document.createElement('span');
			s4.className = 'hidden listId bgChItems';
			s4.innerHTML = OptionData[0];
			liItem.appendChild(s4);
			s5 = document.createElement('span');
			s5.className = 'listS_point right bgChItems';
			s5.title     = 'テーブル名 : ' + OptionData[1] + '\n型 : ' + OptionData[5];
			s5.innerHTML = OptionData[3];
			liItem.appendChild(s5);
			s6 = document.createElement('span');
			s6.className = 'listSize right bgChItems';
			s6.title     = 'テーブル名 : ' + OptionData[1] + '\n型 : ' + OptionData[5];
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

	//さらに候補取得ボタン追加
	if (idName.match(/lTableName/i)){
		button.innerHTML= '<img src="./img/btntbn04-9.png" id="litemAdd" class="itemAdd" alt="候補追加" >';
		var itemAdd = document.getElementById("litemAdd");
	}else{
		button.innerHTML= '<img src="./img/btntbn04-9.png" id="ritemAdd" class="itemAdd" alt="候補追加" >';
		var itemAdd = document.getElementById("ritemAdd");
	}

	addEvent("click", itemAdd , function(event){ return getItemadd(event);});

	//DBより取得した件数を更新
	getStartNum.value = String(parseInt(getStartNum.value) + ValueDataLength);
}

addEvent('load',window,tableNameOnchange);

//F5禁止
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

//モーダル制御のイベント貼り付け
addEvent('load',window,modal_delOpenButton);
addEvent('load',window,modal_delCloseButton);
addEvent("load", window , listCue);
