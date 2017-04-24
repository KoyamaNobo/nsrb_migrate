var newCount  = 1;
var sendFlg   = false;
var csvExeFlg = false;
var interval_id = null;
var errCount = 0;

//登録ボタン押下時のイベントを張り付ける
function signButton(evt){
	var lb = $('.signButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , sign);
	}
}
addEvent('load',window,signButton);

//条件タグ登録リクエスト
function sign() {
	var Command_Name = $('#Command_Name');
	var Sql_str = $('#Sql_str');
	var http1 = createXmlHttpRequest();
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	//入力チェック
	//空白チェック
	if(!input_check_Space(trim(Command_Name[0].value))){
		errJsCode = "ERRJ0052";
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
	//禁止文字チェック
	if(!input_check_symbol(trim(Command_Name[0].value))){
		errJsCode = "ERRJ0053";
		outputMessage(errJsCode,"errMessageModal");
		return false;
	}
		
	
	if(http1) {
		http1.open("POST", "DataView_command_insert.php", true);
		
		http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		http1.onreadystatechange = function() {
			/*  サーバーから応答があった時の処理  */
			if(http1.readyState == 4 && http1.status == 200) {
				writeRes(http1.responseText);
				//モーダルクローズ
				modalClose();
			}else{
				//エラー時の処理
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

//登録結果表示
function writeRes (ReturnValue) {
	//エラーコードが帰ってきたときの処理を追加すること
	messageInput = trim(ReturnValue);
	if( /^ERR(.)*/.test(messageInput)){
		outputMessage(messageInput,"errMessage");
	}else{
		outputMessage(messageInput,"successMessage");
	}
}

//ページリンク押下時のイベントを張り付ける
var linkButton = function () {
	var lb = $('.link');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , link);
	}
}

//ページリンク押下時のイベント
function link(evt){
	var pageNum;
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	if (!sendFlg && !csvExeFlg){
		pageNum = this.className.match( /page=([0-9]+) / );
		if((pageNum.length > 0)){
			
			//フォーム作成
			var sendForm = document.createElement('form');
			sendForm.className = "hidden";
			sendForm.setAttribute('method','post');
			sendForm.setAttribute('action','./DataView_disp.php?'+ pageNum[0]);
			
			//画面の戻り先取得
			var pgName = document.getElementById('prePgName');
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',pgName.name);
			sendInp.setAttribute('value',pgName.value);
			sendForm.appendChild( sendInp );
			
			//条件タグID
			var selectedCommandId = document.getElementById('selectedCommandId');
			if(selectedCommandId){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedCommandId.name);
				sendInp.setAttribute('value',selectedCommandId.value);
				sendForm.appendChild( sendInp );
			}
			
			//選択テーブル
			var selectedTables = document.getElementsByName('selectedTables[]');
			for(var i=0;i < selectedTables.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedTables[i].name);
				sendInp.setAttribute('value',selectedTables[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//結合条件
			var selectedBonds = document.getElementsByName('selectedBonds[]');
			for(var i=0;i < selectedBonds.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedBonds[i].name);
				sendInp.setAttribute('value',selectedBonds[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//表示項目
			var selectedItemNames = document.getElementsByName('selectedItemNames[]');
			for(var i=0;i < selectedItemNames.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedItemNames[i].name);
				sendInp.setAttribute('value',selectedItemNames[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//抽出条件
			var selectedFilters = document.getElementsByName('selectedFilters[]');
			for(var i=0;i < selectedFilters.length ; i++){
				var sendInp = document.createElement('input');
				sendInp.setAttribute('name',selectedFilters[i].name);
				sendInp.setAttribute('value',selectedFilters[i].value);
				sendForm.appendChild( sendInp );
			}
			
			//並べ替え
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


//csv作成ボタン押下時のイベントを張り付ける
function csvButton(evt){
	var lb = $('.csvButton');
	for(var i = 0; i < lb.length ;i++){
		addEvent("click", lb[i] , csv);
	}
}
addEvent('load',window,csvButton);

//ajaxを利用してSCV作成処理を実行させる
function csv(){
	
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	
	errCount = 0;
	
	//実行
	if (!csvExeFlg && !sendFlg){
	
		var Sql_str = $('#Sql_str');
		var Output_file = $('#Output_file');
		var http1 = createXmlHttpRequest();
		if(http1) {
			csvExeFlg = true;
			http1.open("POST", "DataView_create_csv_exe.php", true);
			http1.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
			http1.onreadystatechange = function() {
				/*  サーバーから応答があった時の処理  */
				if(http1.readyState == 4 && http1.status == 200) {
					//成功時の処理
					res = trim(http1.responseText);
					if( /^ERR(.)*/.test(res)){
						outputMessage(res,"errMessage");
					}else{
						str = http1.responseText.split(",");
						//CSV作成しているプロセスIDを取得
						getPID = str[0].replace(/PID:/,"");
						//画面（hidden）プロセスIDを代入
						var PID = $('#PID');
						PID[0].value = getPID;
						//モーダル画面表示
						$('#DLmodalWritingCount')[0].innerHTML = '0' ;
						DLmodalOpen();
						
						//定期的に監視処理を実行させる
						interval_id = setInterval('pid_check();',5000);
					}
				}else{
					//エラー時の処理
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



//pidが実行中かどうか調べる
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
			/*  サーバーから応答があった時の処理  */
			if(http2.readyState == 4 && http2.status == 200) {
				//成功時の処理
				http2.responseText;
				res = trim(http2.responseText);
				if( /^ERR(.)*/.test(res)){
					errCount = errCount + 1;
					if(errCount > 100){
						errJsCode = "ERRJ0057";
						outputMessage("errJsCode","errMessage");
						//CSV作成処理終了
						csvExeFlg = false;
						//モーダル表示を消す
						setTimeout('DLmodalClose()',3000)
						// タイマーを停止する
						clearInterval(interval_id);
					}
				}else{
					str = http2.responseText.split(",");
					//プロセスIDの実行状態取得
					getPIDinfo = str[0].replace(/PIDinfo:/,"");
					getWritingCount = str[1].replace(/WritingCount:/,"");
					//画面にプロセスIDと取得予定件数を代入
					$('#DLmodalWritingCount')[0].innerHTML = getWritingCount ;
					
					//実行中か確認
					if(getPIDinfo != "true"){
						//実行していない場合
						//CSV作成処理終了
						csvExeFlg = false;
						//モーダル表示を消す
						setTimeout('DLmodalClose()',3000)
						// タイマーを停止する
						clearInterval(interval_id);
						//ダウンロード処理を開始する。
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



//ダウンロードモーダルOPEN
function DLmodalOpen(){
	//キーボード操作などにより、オーバーレイが多重起動するのを防止する
	$(this).blur() ;  //ボタンからフォーカスを外す
	
	//モーダルをフェードインさせる
	$("#DLmodal").fadeIn("slow");
	
	//モーダル画面をセンタリング
	centeringModalSyncer("#DLmodalBody");
}


//ダウンロードモーダル閉じる
function DLmodalClose(){
	//キーボード操作などにより、オーバーレイが多重起動するのを防止する
	$(this).blur() ;	//ボタンからフォーカスを外す

	//[$modal]をフェードインさせる
	$("#DLmodal").fadeOut("slow");
}



//SCVをダウンロードする
function download_csv(){
	//メッセージ初期化
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	outputMessage("","errMessageModal");
	//フォーム作成
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_download.php');
	//出力先ファイル名取得
	var Output_file = document.getElementById('Output_file');
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',Output_file.name);
	sendInp.setAttribute('value',Output_file.value);
	sendForm.appendChild( sendInp );
	//フォーム実行
	document.body.appendChild( sendForm );
	sendForm.submit();
}
