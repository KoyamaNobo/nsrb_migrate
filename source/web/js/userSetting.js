//blinkの点滅時間
var blinkTime = 50;
//ユーザ設定を適用
var firstSetting = function(){
	var sfc  = document.getElementById("sfcolor").value;

	//強調表示の設定
	var strStrong = '<style> .nextinput:focus{';
	strStrong += 'color:' + sfc + ' !important;';
	strStrong += '}</style>';
	 $('head').append(strStrong);

	userSetting();
	blinkCon();
}

var userSetting = function () {
	var fc  = document.getElementById("fcolor").value;
	var fs  = document.getElementById("fsize").value;
	var bgc = document.getElementById("bgcolor").value;

	//背景色の変更
	{
		$('.screen,.status').css('background-color',bgc);
	}

	//文字の大きさ変更
	{
		$('.contentbody,.status').css('font-size',fs + "px");
	}

	//文字色の変更
	{
		$('.screen,.status').find('a').css('color',fc);
		$('.screen,.status').find('span').css('color',fc);
		$('.screen,.status').find('input').css('color',fc);
	}

	//REVERSEの設定
	{
		$('.reverse').css('color',bgc);
		$('.reverse').css('background-color',fc);
	}
}

var prevblinkTime = new Date().getTime();
var blinkVisibleFlg = true;
	//ここのNameSpace内だけでいいはず
var blinkCon =function (){
		//IE対応だったものをJQueryで書き直す
		var targetArray = new Array();
		// var visibleFlg = true;
		targetArray = $('.blink');
		if(targetArray.length > 0){
			if(blinkVisibleFlg != false){
				if(prevblinkTime + 700 <  new Date().getTime()){
					for(var i = 0;i < targetArray.length;i++){
						targetArray[i].style.visibility = "hidden";
						// visibleFlg = false;
						prevblinkTime = new Date().getTime();
						blinkVisibleFlg = false;
					}
				}
			}else{
				if(prevblinkTime + 300 < new Date().getTime()){
					for(var i = 0;i < targetArray.length;i++){
						targetArray[i].style.visibility = "visible";
						prevblinkTime = new Date().getTime();
						blinkVisibleFlg = true;
					}
				}
			}
		}
		setTimeout(blinkCon,blinkTime);
}

addEvent('load',window,blinkCon);
addEvent('load',window,firstSetting);
