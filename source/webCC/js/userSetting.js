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
		// $('input.nextinput:focus').css('background-color',document.getElementById("sbgcolor").value);
	}

	//文字の大きさ変更
	{
		$('.contentbody,.status').css('font-size',fs + "px");
	}

	//文字色の変更
	{
		$('.screen,.status').find('a').css('color',fc);
		$('.screen').find('span').css('color',fc);
		//中の子は入れ替わるので
		$('.status').find('div').css('color',fc);
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
		if(blinkVisibleFlg != false){
			if(prevblinkTime + 700 <  new Date().getTime()){
				var targetArray = $('.blink');
				for(var i = 0;i < targetArray.length;i++){
					targetArray[i].style.visibility = "hidden";
					//prevblinkTime = new Date().getTime();
					blinkVisibleFlg = false;
				}
				// 点滅表示の設定はinput.jsでも行っているので、ここでは項目の有無に関わらず必ず時間をリセットする。(blink項目が現れてからすぐにはここの処理は実行されなくなる。)
				// リセットしないとblinkTimeごとにDOMにアクセスすることになる。
				prevblinkTime = new Date().getTime();
			}
		}else{
			if(prevblinkTime + 300 < new Date().getTime()){
				var targetArray = $('.blink');
				for(var i = 0;i < targetArray.length;i++){
					targetArray[i].style.visibility = "visible";
					//prevblinkTime = new Date().getTime();
					blinkVisibleFlg = true;
				}
				// 点滅表示の設定はinput.jsでも行っているので、ここでは項目の有無に関わらず必ず時間をリセットする。(blink項目が現れてからすぐにはここの処理は実行されなくなる。)
				// リセットしないとblinkTimeごとにDOMにアクセスすることになる。
				prevblinkTime = new Date().getTime();
			}
		}
		setTimeout(blinkCon,blinkTime);
}

addEvent('load',window,blinkCon);
addEvent('load',window,firstSetting);
