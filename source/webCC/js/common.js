/*
共通js領域
Create:20140111
Author:n.koyama
必要な関数があれば下のコメントアウトから上に切り出して使う
統一ブラウザにはなるが汎用設定もしておく
*/


//
//モーダルウィンドウ
//********************* input ***********:
//********************* return **********:
//********************* create **********:
//date    :20140111
//Author  :n.koyama
$(function(){
    // 「.modal-open」をクリック
    $('.modal-open').click(screensSelect);

});

var selectScreenClose = function(){
    // モーダルコンテンツとオーバーレイをフェードアウト
    $(modal).fadeOut('slow');
    $('.modal-overlay').fadeOut('slow',function(){
        // オーバーレイを削除
        $('.modal-overlay').remove();
    });
}

var screensSelect = function(){
	//
	$('#modal')[0].style.display = 'block';
    // オーバーレイ用の要素を追加
    $('body').append('<div class="modal-overlay"></div>');
    // オーバーレイをフェードイン
    $('.modal-overlay').fadeIn('slow');

    // モーダルコンテンツのIDを取得
    var modal = '#modalBody';
    // モーダルコンテンツの表示位置を設定
    modalResize();
     // モーダルコンテンツフェードイン
    $(modal).fadeIn('slow');

    // 「.modal-overlay」あるいは「.modal-close」をクリック
    $('.modal-overlay, .modal-close').off().click(selectScreenClose);

    // リサイズしたら表示位置を再取得
    $(window).on('resize', function(){
        modalResize();
    });

    // モーダルコンテンツの表示位置を設定する関数
    function modalResize(){
        // ウィンドウの横幅、高さを取得
        var w = $(window).width();
        var h = $(window).height();

        // モーダルコンテンツの表示位置を取得
        var x = (w - $(modal).outerWidth(true)) / 2;
        var y = (h - $(modal).outerHeight(true)) / 2;
        
        // モーダルコンテンツの表示位置を設定
        $(modal).css({'left': x + 'px','top': y + 'px'});
    }
}

//
//特定のエレメントにイベントを追加
//********************* input ***********:
//evt  : イベント名
//elem : 対象のエレメント
//fn   : ファンクション(クロージャか直置き)
//********************* return **********:
//********************* create **********:
//date    :20140111
//Author  :n.koyama
function addEvent(evt , elem , fn){
	try {
		elem.addEventListener( evt, fn, false);
	} catch (e) {
		elem.attachEvent('on' + evt, fn);
	}
}

//
//特定のエレメントにイベントを追加
//********************* input ***********:
//elem      : ターゲットとしてのrootエレメント
//classname : 対象とするクラス名
//********************* return **********:
//targElem  :取得されたエレメントのarray
//********************* create **********:
//date    :20140111
//Author  :n.koyama
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

//
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

//
//elementの先祖で一番近い特定のクラスがついているelementを返す
//********************* input ***********:
//elem:対象となるelement
//********************* return **********:
//classname:探す対象となるクラス名
//********************* create **********:
//date    :不明
//Author  :Y.hagihara
function findParentByClassName( elem, classname ){

	var targ =  elem.parentNode;
	if( targ ){
		if( targ.nodeType == 1 ){
			if( !existClassName( targ , classname) ){
				targ = findParentByClassName( targ, classname);
			}
		}
	}
	return targ;
}

//
//elementにclassnameがついているかどうかを判定
//********************* input ***********:
//elem:対象となるelement
//********************* return **********:
//classname:探す対象となるクラス名
//********************* create **********:
//date    :不明
//Author  :Y.hagihara
function existClassName( elem, classname ){

	var exist = false;

	var attrString = elem.className;

	if(attrString){
		var attrArray = attrString.split(' ');
		if(attrArray){
			for(let jj = 0;jj < attrArray.length; jj++){
				if(attrArray[jj] == classname){
					exist = true;
				}
			}
		}
	}

	return exist;

}

/*
function setClass( elem, classname){

	function trim( str ){
		return str.replace(/(^\s+)|(\s+$)/g, "");
	}

	var attrString = elem.className;

	var tmpClasses = '';
	if(attrString){
		var attrArray = attrString.split(' ');
		if(attrArray){
			var exist = false;
			for(var j = 0;j < attrArray.length; j++){
				if(attrArray[j] == classname){
					exist = true;
				}
			}
			if( !exist ){
				attrArray.push( classname );
			}
		}else{
			attrArray.push( classname );
		}

		for( i= 0 ;i < attrArray.length; i++  ){
			tmpClasses += attrArray[i] + ' ';
		}
	}else{
		tmpClasses =  trim( classname );
	}

 	elem.className = tmpClasses;
}

function removeClass( elem, classname){

	function trim( str ){
		return str.replace(/(^\s+)|(\s+$)/g, "");
	}

	var attrString = elem.className;

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

		 	elem.className = tmpClasses;
		}
	}
}


function findParent ( elem, tagname ){

	var targ =  elem.parentNode;
	if( targ ){
		if( targ.nodeType == 1 ){
			if( targ.nodeName != tagname  ){
				targ = findParent( targ, tagname);
			}
		}
	}
	return targ;
}


var iframeHeight = function( params ){
	if(!window.parent.document) return false;

	try { // !IE
		document.styleSheets[0].insertRule( 'html' + '{overflow:hidden;}', document.styleSheets[0].cssRules.length );
	} catch (e) { // IE
		document.styleSheets[0].addRule( 'html', '{overflow:hidden;}');
	}
	height = document.body.scrollHeight + (document.body.offsetHeight - document.body.clientHeight);
	window.parent.document.getElementById( params ).style.height = height + 0 + 'px';

	setTimeout( function(){ iframeHeight( params ); } ,750)
}


var setHoverRowbgColor = function(targ ,evtBindClass, chgColorClass ,addClass ,exemptClass){

	var targ = document.getElementById( targ );

	var trs  = getElementsByClassName( targ , evtBindClass);
	for( k= 0 ;k < trs.length; k++  ){
		addEvent('mouseover',trs[k], (function( bindC , chgC ,addC , exemptC){
			return function(evt){
				if ( navigator.userAgent.indexOf('MSIE') != -1 ){
					var ieParent = findParentByClassName(window.event.srcElement , bindC );
					var childs = getElementsByClassName( ieParent , chgC );
				}else{
					var childs = getElementsByClassName( evt.currentTarget , chgC );
				}
				for( l= 0 ;l < childs.length; l++ ){
					if(childs[l].nodeType  == 1){  // Node.ELEMENT_NODE=1 
						if( !existClassName(childs[l], exemptC  ) ){
							setClass(childs[l] , addC);
						}
					}
				}
			}
		})( evtBindClass, chgColorClass , addClass ,exemptClass) );
	}

}

var removeHoverRowbgColor = function(targID ,evtBindClass, chgColorClass ,addClass ,exemptClass){

	var targ = document.getElementById( targID );
	var trs  = getElementsByClassName( targ , evtBindClass);

	for( k= 0 ;k < trs.length; k++  ){
		addEvent('mouseout',trs[k],( function( bindC , chgC ,addC , exemptC){
			return function(evt){
				if ( navigator.userAgent.indexOf('MSIE') != -1 ){
					var ieParent = findParentByClassName(window.event.srcElement , bindC );
					var childs = getElementsByClassName( ieParent , chgC );
				}else{
					var childs = getElementsByClassName( evt.currentTarget , chgC );
				}
				for( l= 0 ;l < childs.length; l++ ){
					if(childs[l].nodeType  == 1){  // Node.ELEMENT_NODE=1 
						if( !existClassName(childs[l], exemptC ) ){
							removeClass(childs[l] , addC );
						}
					}
				}
			}
		})( evtBindClass, chgColorClass , addClass ,exemptClass) );
	}

}

var setHeight = function( srcElemID, targElemClassName ,heightSetElemClassName ){

	var src = document.getElementById( srcElemID );

	if( src ){
		var targElems = getElementsByClassName(document, targElemClassName );
		if( targElems ){
			for( var i=0;i < targElems.length;i++ ){
				var  heightSetElems =  getElementsByClassName( targElems[i], heightSetElemClassName);
				if( heightSetElems ){

					for( var j=0;j < heightSetElems.length;j++ ){
						heightSetElems[i].style.height = src.scrollHeight + 'px';
					}
				}
			}
		}
	}
}

var setHigerHeight = function( srcElemID, targElemID ){

	var src = document.getElementById( srcElemID );
	var targ = document.getElementById( targElemID );

	if( src ){
		if( targ ){
			if( src.scrollHeight > targ.scrollHeight ){
				targ.style.height = src.scrollHeight + 'px';
			}else{
				src.style.height = targ.scrollHeight + 'px'; 
			}
		}
	}
}

var setWidth4SumChildrenWidth= function( targId, getBaseElemClassName , getWidthTagName , SetArrays ,LRmarginStyle){
	var targ = document.getElementById( targId );

	if( targ ){
		var setElems = getElementsByClassName( targ , getBaseElemClassName );
	}else{
		var setElems = getElementsByClassName( document , getBaseElemClassName );
	}

	for( k= 0 ;k < setElems.length; k++ ){
		var elems =	setElems[k].getElementsByTagName( getWidthTagName );
		var width = 0;
		for( i= 0 ;i < elems.length; i++ ){

			var style = elems[i].currentStyle || document.defaultView.getComputedStyle(elems[i], '')
			var margLeftStyle = style.marginLeft;
			var margRightStyle = style.marginRight;
			var paddLeftStyle = style.paddingLeft;
			var paddRightStyle = style.paddingRight;

			var tmp  =  parseInt( margLeftStyle.replace( LRmarginStyle , '') ) + elems[i].offsetWidth ;
			tmp += parseInt( margLeftStyle.replace( LRmarginStyle, ''));
			tmp += parseInt( margRightStyle.replace( LRmarginStyle, ''));
			tmp += parseInt( paddLeftStyle.replace( LRmarginStyle, ''));
			tmp += parseInt( paddRightStyle.replace( LRmarginStyle, ''));

			width += tmp;
		}

		if( SetArrays ){
			for( j= 0 ;j < SetArrays.length; j++ ){
				var widSetelem = getElementsByClassName( targ , SetArrays[j] )	
				for( m = 0 ;m < widSetelem.length; m++ ){
					widSetelem[m].style.width  = width + 0 + 'px';
				}	
			}
		}
	}

}
*/